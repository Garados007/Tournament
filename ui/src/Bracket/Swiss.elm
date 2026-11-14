module Bracket.Swiss exposing
    ( Game
    , Match
    , Msg(..)
    , Player
    , Round
    , Settings
    , Standing
    , State
    , addPlayer
    , autoRounds
    , canStartNextRound
    , currentRound
    , defaultSettings
    , isEditable
    , isFinished
    , isRemoved
    , matchCell
    , pairRound
    , placements
    , removePlayer
    , standings
    , start
    , update
    )

{-| A fixed number of rounds. In every round players with similar match points
play each other, but never twice against the same opponent.
-}

import Bracket.Ranking as Ranking exposing (Criterion)
import Bracket.Scoring as Scoring exposing (Cell(..), Outcome(..), Scoring(..))
import Dict exposing (Dict)
import List.Extra
import Placement exposing (Placement)
import PlayerId exposing (PlayerId)
import Set exposing (Set)


{-| `rounds` is `Nothing` for the automatic number of rounds.
-}
type alias Settings =
    { rounds : Maybe Int
    , scoring : Scoring
    }


defaultSettings : Settings
defaultSettings =
    { rounds = Nothing
    , scoring = Points
    }


{-| A removed player isn't paired again and has no place.
-}
type alias Player =
    { id : PlayerId
    , removed : Bool
    }


{-| The scores are in the order of `home` and `away`.
-}
type alias Match =
    { home : PlayerId
    , away : PlayerId
    , result : Maybe ( Int, Int )
    }


type alias Round =
    { matches : List Match
    , bye : Maybe PlayerId
    }


{-| `players` are in start order, added players at the end. `rounds` are in the
order they were played; the last one is the current round. `totalRounds` is
fixed at the start.
-}
type alias Game =
    { players : List Player
    , totalRounds : Int
    , rounds : List Round
    }


type alias State =
    { settings : Settings
    , game : Game
    }


{-| The home player comes first, the away player second. The scores are in the
same order.
-}
type Msg
    = SetResult PlayerId PlayerId ( Int, Int )
    | ClearResult PlayerId PlayerId
    | NextRound


{-| The smallest number of rounds after which only one player can have won
every match.
-}
autoRounds : Int -> Int
autoRounds players =
    let
        go rounds =
            if 2 ^ rounds >= players then
                rounds

            else
                go (rounds + 1)
    in
    go 0


start : Settings -> List PlayerId -> State
start settings ids =
    let
        count =
            List.length ids
    in
    { settings = settings
    , game =
        { players = List.map (\id -> Player id False) ids
        , totalRounds =
            Maybe.withDefault (autoRounds count) settings.rounds
                |> clamp 1 (max 1 (count - 1))
        , rounds = [ firstRound ids ]
        }
    }


{-| The top half of the start order plays against the bottom half. With an odd
number of players the last one has a bye.
-}
firstRound : List PlayerId -> Round
firstRound ids =
    let
        ( paired, bye ) =
            if modBy 2 (List.length ids) == 1 then
                ( List.take (List.length ids - 1) ids, List.Extra.last ids )

            else
                ( ids, Nothing )

        ( top, bottom ) =
            List.Extra.splitAt (List.length paired // 2) paired
    in
    { matches = List.map2 (\home away -> Match home away Nothing) top bottom
    , bye = bye
    }


mapGame : (Game -> Game) -> State -> State
mapGame mapper state =
    { state | game = mapper state.game }


{-| Added players are paired from the next round on.
-}
addPlayer : PlayerId -> State -> State
addPlayer id =
    mapGame <|
        \game ->
            if List.any (.id >> (==) id) game.players then
                game

            else
                { game | players = game.players ++ [ Player id False ] }


{-| An open match of the player is a forfeit win for the opponent.
-}
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


currentRound : Game -> Maybe Round
currentRound game =
    List.Extra.last game.rounds


{-| The match from the view of the home player.
-}
matchCell : Game -> Match -> Cell
matchCell game match =
    case match.result of
        Just score ->
            Played score

        Nothing ->
            case ( isRemoved match.home game, isRemoved match.away game ) of
                ( False, True ) ->
                    Forfeit True

                ( True, False ) ->
                    Forfeit False

                ( True, True ) ->
                    Cancelled

                ( False, False ) ->
                    Open


{-| Only matches of the current round without a removed player can be changed.
-}
isEditable : PlayerId -> PlayerId -> Game -> Bool
isEditable home away game =
    not (isRemoved home game)
        && not (isRemoved away game)
        && (currentRound game
                |> Maybe.map (.matches >> List.any (\match -> match.home == home && match.away == away))
                |> Maybe.withDefault False
           )


isRoundComplete : Game -> Round -> Bool
isRoundComplete game round =
    List.all (\match -> matchCell game match /= Open) round.matches


isCurrentRoundComplete : Game -> Bool
isCurrentRoundComplete game =
    currentRound game
        |> Maybe.map (isRoundComplete game)
        |> Maybe.withDefault True


canStartNextRound : Game -> Bool
canStartNextRound game =
    isCurrentRoundComplete game && List.length game.rounds < game.totalRounds


isFinished : Game -> Bool
isFinished game =
    isCurrentRoundComplete game && List.length game.rounds >= game.totalRounds


update : Msg -> State -> State
update msg state =
    case msg of
        SetResult home away score ->
            setResult home away (Just <| Scoring.normalize state.settings.scoring score) state

        ClearResult home away ->
            setResult home away Nothing state

        NextRound ->
            if canStartNextRound state.game then
                mapGame
                    (\game ->
                        case pairNextRound state of
                            Just round ->
                                { game | rounds = game.rounds ++ [ round ] }

                            Nothing ->
                                -- nobody can be paired anymore, so this was the last round
                                { game | totalRounds = List.length game.rounds }
                    )
                    state

            else
                state


setResult : PlayerId -> PlayerId -> Maybe ( Int, Int ) -> State -> State
setResult home away result state =
    if isEditable home away state.game then
        mapGame
            (\game ->
                { game
                    | rounds =
                        List.Extra.updateAt (List.length game.rounds - 1)
                            (\round ->
                                { round
                                    | matches =
                                        List.map
                                            (\match ->
                                                if match.home == home && match.away == away then
                                                    { match | result = result }

                                                else
                                                    match
                                            )
                                            round.matches
                                }
                            )
                            game.rounds
                }
            )
            state

    else
        state


{-| The opponents of a player with the cells from the player's view. A bye has
no opponent and is a win.
-}
entriesOf : PlayerId -> Game -> List ( Maybe PlayerId, Cell )
entriesOf id game =
    List.concatMap
        (\round ->
            (if round.bye == Just id then
                [ ( Nothing, Forfeit True ) ]

             else
                []
            )
                ++ List.filterMap
                    (\match ->
                        if match.home == id then
                            Just ( Just match.away, matchCell game match )

                        else if match.away == id then
                            Just ( Just match.home, Scoring.flip <| matchCell game match )

                        else
                            Nothing
                    )
                    round.matches
        )
        game.rounds


{-| Removed players have no place. `buchholz` is the sum of the match points of
all opponents.
-}
type alias Standing =
    { id : PlayerId
    , wins : Int
    , draws : Int
    , losses : Int
    , scored : Int
    , conceded : Int
    , matchPoints : Int
    , buchholz : Int
    , place : Maybe Int
    }


matchPointsOf : List Cell -> Int
matchPointsOf =
    List.filterMap Scoring.cellOutcome
        >> List.map Scoring.matchPoints
        >> List.sum


standingOf : Game -> Dict PlayerId Int -> PlayerId -> Standing
standingOf game points id =
    let
        entries =
            entriesOf id game

        outcomes =
            List.filterMap (Tuple.second >> Scoring.cellOutcome) entries

        count outcome =
            List.length <| List.filter ((==) outcome) outcomes

        scores =
            List.filterMap
                (\( _, c ) ->
                    case c of
                        Played score ->
                            Just score

                        _ ->
                            Nothing
                )
                entries
    in
    { id = id
    , wins = count Win
    , draws = count Draw
    , losses = count Loss
    , scored = List.sum <| List.map Tuple.first scores
    , conceded = List.sum <| List.map Tuple.second scores
    , matchPoints = List.sum <| List.map Scoring.matchPoints outcomes
    , buchholz =
        List.filterMap Tuple.first entries
            |> List.filterMap (\opponent -> Dict.get opponent points)
            |> List.sum
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
            entriesOf standing.id state.game
                |> List.filter (\( opponent, _ ) -> List.any (\other -> Just other.id == opponent) tied)
                |> List.map Tuple.second
                |> matchPointsOf
    in
    [ overall .matchPoints
    , overall .buchholz
    ]
        ++ (case state.settings.scoring of
                Points ->
                    [ overall <| \s -> s.scored - s.conceded
                    , overall .scored
                    ]

                WinnerOnly ->
                    []
           )
        ++ [ headToHead
           , overall .wins
           ]


{-| The live table of the players in rank order. Removed players come last.
Tied players share a place and keep the start order.
-}
standings : State -> List Standing
standings state =
    let
        game =
            state.game

        points =
            game.players
                |> List.map (\player -> ( player.id, matchPointsOf <| List.map Tuple.second <| entriesOf player.id game ))
                |> Dict.fromList

        ( removed, active ) =
            List.partition .removed game.players
    in
    (List.map (.id >> standingOf game points) active
        |> Ranking.rank (criteria state)
        |> List.map (\( place, standing ) -> { standing | place = Just place })
    )
        ++ List.map (.id >> standingOf game points) removed


{-| The places 1 to 3 once the last round has all results.
-}
placements : State -> List Placement
placements state =
    if isFinished state.game then
        standings state
            |> List.map (\standing -> ( standing.place, standing.id ))
            |> Ranking.placements

    else
        []



-- PAIRING


pairKey : PlayerId -> PlayerId -> ( PlayerId, PlayerId )
pairKey a b =
    ( min a b, max a b )


{-| The next round of the active players in rank order. Pairs that already
played aren't allowed again.
-}
pairNextRound : State -> Maybe Round
pairNextRound state =
    let
        played : Set ( PlayerId, PlayerId )
        played =
            state.game.rounds
                |> List.concatMap .matches
                |> List.map (\match -> pairKey match.home match.away)
                |> Set.fromList

        hadBye =
            List.filterMap .bye state.game.rounds
    in
    standings state
        |> List.filter (\standing -> standing.place /= Nothing)
        |> List.map .id
        |> pairRound
            { allowed = \a b -> not <| Set.member (pairKey a b) played
            , hadBye = \id -> List.member id hadBye
            }


{-| Pairs the players, given in rank order, from the top: the first player gets
the highest-ranked allowed opponent with whom the rest can still be paired.

With an odd number the lowest-ranked player without a bye gets one; if everyone
had a bye, the lowest-ranked player. A higher player takes the bye only if the
others can't be paired otherwise.

`Nothing` if fewer than 2 players are left or no pairing exists.

-}
pairRound : { allowed : PlayerId -> PlayerId -> Bool, hadBye : PlayerId -> Bool } -> List PlayerId -> Maybe Round
pairRound { allowed, hadBye } ranked =
    let
        toRound bye pairs =
            { matches = List.map (\( home, away ) -> Match home away Nothing) pairs
            , bye = bye
            }

        tryByes candidates failed =
            case candidates of
                [] ->
                    Nothing

                bye :: rest ->
                    case pairAll allowed (List.filter ((/=) bye) ranked) failed of
                        ( Just pairs, _ ) ->
                            Just <| toRound (Just bye) pairs

                        ( Nothing, stillFailed ) ->
                            tryByes rest stillFailed
    in
    if List.length ranked < 2 then
        Nothing

    else if modBy 2 (List.length ranked) == 1 then
        List.reverse ranked
            |> List.partition (not << hadBye)
            |> (\( fresh, again ) -> fresh ++ again)
            |> (\candidates -> tryByes candidates Set.empty)

    else
        pairAll allowed ranked Set.empty
            |> Tuple.first
            |> Maybe.map (toRound Nothing)


{-| `failed` remembers the lists of players that can't be paired, so no list is
tried twice.
-}
pairAll : (PlayerId -> PlayerId -> Bool) -> List PlayerId -> Set (List PlayerId) -> ( Maybe (List ( PlayerId, PlayerId )), Set (List PlayerId) )
pairAll allowed players failed =
    case players of
        [] ->
            ( Just [], failed )

        first :: rest ->
            if Set.member players failed then
                ( Nothing, failed )

            else
                let
                    try candidates stillFailed =
                        case candidates of
                            [] ->
                                ( Nothing, Set.insert players stillFailed )

                            opponent :: others ->
                                if allowed first opponent then
                                    case pairAll allowed (List.filter ((/=) opponent) rest) stillFailed of
                                        ( Just pairs, newFailed ) ->
                                            ( Just <| ( first, opponent ) :: pairs, newFailed )

                                        ( Nothing, newFailed ) ->
                                            try others newFailed

                                else
                                    try others stillFailed
                in
                try rest failed
