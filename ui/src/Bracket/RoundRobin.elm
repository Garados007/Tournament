module Bracket.RoundRobin exposing
    ( Game
    , Matches(..)
    , Msg(..)
    , Player
    , Ranking(..)
    , Settings
    , Standing
    , State
    , addPlayer
    , cell
    , defaultSettings
    , effectiveRanking
    , isEditable
    , isFinished
    , isRemoved
    , removePlayer
    , resultKey
    , standings
    , start
    , toStandings
    , update
    )

{-| Every player plays against every other player. The table is ranked by the
results.
-}

import Bracket.Ranking as Ranking exposing (Criterion)
import Bracket.Scoring as Scoring exposing (Cell(..), Outcome(..), Scoring(..))
import Dict exposing (Dict)
import PlayerId exposing (PlayerId)
import Standings exposing (Standings)


type Matches
    = Single
    | Rematch


type Ranking
    = ByResults
    | ByTotalPoints


type alias Settings =
    { matches : Matches
    , scoring : Scoring
    , ranking : Ranking
    }


defaultSettings : Settings
defaultSettings =
    { matches = Single
    , scoring = Points
    , ranking = ByResults
    }


{-| The ranking only `Points` can use.
-}
effectiveRanking : Settings -> Ranking
effectiveRanking settings =
    case settings.scoring of
        Points ->
            settings.ranking

        WinnerOnly ->
            ByResults


{-| A removed player left the tournament. Their unplayed matches are lost.
-}
type alias Player =
    { id : PlayerId
    , removed : Bool
    }


{-| `players` are in start order. A result is keyed by the home and away
player and holds their scores in that order. With `WinnerOnly` the scores are
1:0, 0:0 or 0:1. With `Single` only the key with the lower id is used.
-}
type alias Game =
    { players : List Player
    , results : Dict ( PlayerId, PlayerId ) ( Int, Int )
    }


type alias State =
    { settings : Settings
    , game : Game
    }


{-| The row player comes first, the column player second. The scores are in
the same order.
-}
type Msg
    = SetResult PlayerId PlayerId ( Int, Int )
    | ClearResult PlayerId PlayerId


start : Settings -> List PlayerId -> State
start settings ids =
    { settings = settings
    , game =
        { players = List.map (\id -> Player id False) ids
        , results = Dict.empty
        }
    }


mapGame : (Game -> Game) -> State -> State
mapGame mapper state =
    { state | game = mapper state.game }


addPlayer : PlayerId -> State -> State
addPlayer id =
    mapGame <|
        \game ->
            if List.any (.id >> (==) id) game.players then
                game

            else
                { game | players = game.players ++ [ Player id False ] }


removePlayer : PlayerId -> State -> State
removePlayer id =
    mapGame <|
        \game ->
            { game
                | players =
                    List.map
                        (\player ->
                            if player.id == id then
                                { player | removed = True }

                            else
                                player
                        )
                        game.players
            }


isRemoved : PlayerId -> Game -> Bool
isRemoved id game =
    List.any (\player -> player.id == id && player.removed) game.players


isPlayer : PlayerId -> Game -> Bool
isPlayer id game =
    List.any (.id >> (==) id) game.players


{-| Matches with a removed player are final: clearing a played one would turn
it into a forfeit.
-}
isEditable : PlayerId -> PlayerId -> Game -> Bool
isEditable row column game =
    row
        /= column
        && isPlayer row game
        && isPlayer column game
        && not (isRemoved row game)
        && not (isRemoved column game)


{-| Where a result is stored, and whether its scores are in reverse order.
-}
resultKey : Matches -> PlayerId -> PlayerId -> ( ( PlayerId, PlayerId ), Bool )
resultKey mode row column =
    case mode of
        Single ->
            if row > column then
                ( ( column, row ), True )

            else
                ( ( row, column ), False )

        Rematch ->
            ( ( row, column ), False )


swap : ( Int, Int ) -> ( Int, Int )
swap ( a, b ) =
    ( b, a )


cell : PlayerId -> PlayerId -> State -> Cell
cell row column state =
    let
        ( key, swapped ) =
            resultKey state.settings.matches row column
    in
    case Dict.get key state.game.results of
        Just score ->
            Played <|
                if swapped then
                    swap score

                else
                    score

        Nothing ->
            case ( isRemoved row state.game, isRemoved column state.game ) of
                ( False, True ) ->
                    Forfeit True

                ( True, False ) ->
                    Forfeit False

                ( True, True ) ->
                    Cancelled

                ( False, False ) ->
                    Open


update : Msg -> State -> State
update msg state =
    case msg of
        SetResult row column score ->
            if isEditable row column state.game then
                let
                    ( key, swapped ) =
                        resultKey state.settings.matches row column

                    stored =
                        Scoring.normalize state.settings.scoring <|
                            if swapped then
                                swap score

                            else
                                score
                in
                mapGame (\game -> { game | results = Dict.insert key stored game.results }) state

            else
                state

        ClearResult row column ->
            if isEditable row column state.game then
                mapGame
                    (\game ->
                        { game
                            | results =
                                Dict.remove
                                    (Tuple.first <| resultKey state.settings.matches row column)
                                    game.results
                        }
                    )
                    state

            else
                state


{-| Every match as the home and the away player.
-}
allMatches : State -> List ( PlayerId, PlayerId )
allMatches state =
    let
        ids =
            List.map .id state.game.players
    in
    List.concatMap
        (\row ->
            List.filterMap
                (\column ->
                    if row == column then
                        Nothing

                    else if state.settings.matches == Rematch || row < column then
                        Just ( row, column )

                    else
                        Nothing
                )
                ids
        )
        ids


isFinished : State -> Bool
isFinished state =
    List.all (\( row, column ) -> cell row column state /= Open) (allMatches state)


{-| Removed players have no place.
-}
type alias Standing =
    { id : PlayerId
    , wins : Int
    , draws : Int
    , losses : Int
    , scored : Int
    , conceded : Int
    , matchPoints : Int
    , place : Maybe Int
    }


{-| The cells of all matches of the player against the given opponents, from
the player's view.
-}
cellsOf : PlayerId -> List PlayerId -> State -> List Cell
cellsOf id opponents state =
    List.concatMap
        (\opponent ->
            if opponent == id then
                []

            else
                case state.settings.matches of
                    Single ->
                        [ cell id opponent state ]

                    Rematch ->
                        [ cell id opponent state
                        , cell opponent id state |> Scoring.flip
                        ]
        )
        opponents


matchPointsAgainst : List PlayerId -> State -> PlayerId -> Int
matchPointsAgainst opponents state id =
    cellsOf id opponents state
        |> List.filterMap Scoring.cellOutcome
        |> List.map Scoring.matchPoints
        |> List.sum


standingOf : State -> PlayerId -> Standing
standingOf state id =
    let
        cells =
            cellsOf id (List.map .id state.game.players) state

        outcomes =
            List.filterMap Scoring.cellOutcome cells

        count outcome =
            List.length <| List.filter ((==) outcome) outcomes

        scores =
            List.filterMap
                (\c ->
                    case c of
                        Played score ->
                            Just score

                        _ ->
                            Nothing
                )
                cells
    in
    { id = id
    , wins = count Win
    , draws = count Draw
    , losses = count Loss
    , scored = List.sum <| List.map Tuple.first scores
    , conceded = List.sum <| List.map Tuple.second scores
    , matchPoints = List.sum <| List.map Scoring.matchPoints outcomes
    , place = Nothing
    }


criteria : State -> List (Criterion Standing)
criteria state =
    let
        overall : (Standing -> Int) -> Criterion Standing
        overall f _ =
            f

        headToHead : Criterion Standing
        headToHead tied standing =
            matchPointsAgainst (List.map .id tied) state standing.id
    in
    case ( effectiveRanking state.settings, state.settings.scoring ) of
        ( ByResults, Points ) ->
            [ overall .matchPoints
            , overall <| \s -> s.scored - s.conceded
            , overall .scored
            , headToHead
            , overall .wins
            ]

        ( ByResults, WinnerOnly ) ->
            [ overall .matchPoints
            , headToHead
            , overall .wins
            ]

        ( ByTotalPoints, _ ) ->
            [ overall .scored
            , overall <| \s -> negate s.conceded
            , headToHead
            ]


{-| The live table of the players in rank order. Removed players come last.
Tied players share a place, and the next place is skipped for each of them.
-}
standings : State -> List Standing
standings state =
    let
        ( removed, active ) =
            state.game.players
                |> List.partition .removed
    in
    (List.map (.id >> standingOf state) active
        |> Ranking.rank (criteria state)
        |> List.map (\( place, standing ) -> { standing | place = Just place })
    )
        ++ List.map (.id >> standingOf state) removed


{-| The live table for the leaderboard. A forfeit counts as a win or a loss.
-}
toStandings : State -> Standings
toStandings state =
    let
        withPoints =
            state.settings.scoring == Points

        withMatchPoints =
            effectiveRanking state.settings == ByResults
    in
    { rows =
        List.map
            (\standing ->
                let
                    base =
                        Standings.row
                            { id = standing.id
                            , place = standing.place
                            , alive = True
                            , removed = isRemoved standing.id state.game
                            }
                in
                { base
                    | won = standing.wins
                    , drawn = standing.draws
                    , lost = standing.losses
                    , extra =
                        (if withPoints then
                            [ String.fromInt standing.scored ++ " : " ++ String.fromInt standing.conceded ]

                         else
                            []
                        )
                            ++ (if withMatchPoints then
                                    [ String.fromInt standing.matchPoints ]

                                else
                                    []
                               )
                }
            )
            (standings state)
    , finished = isFinished state
    , liveRanking = True
    , draws = True
    , columns =
        (if withPoints then
            [ "Score" ]

         else
            []
        )
            ++ (if withMatchPoints then
                    [ "Pts" ]

                else
                    []
               )
    }
