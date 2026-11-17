module Bracket.DoubleElimination exposing
    ( Settings
    , State
    , acceptsPlayers
    , addPlayer
    , build
    , defaultSettings
    , dropIndex
    , grandFinal
    , isEliminated
    , isLoserRound
    , isRemoved
    , isResetSkipped
    , isWinnerRound
    , loserRound
    , matchLength
    , playable
    , removePlayer
    , resetMatch
    , resetNeeded
    , start
    , startSeeded
    , toStandings
    , update
    , winnerRound
    )

{-| Every player is out after their second loss. The first loss moves a player
from the winner bracket to the loser bracket. The winners of both brackets meet
in the grand final.

The tournament is a knockout game with all matches in one graph:

  - The winner bracket is a knockout bracket, one round higher than in a
    knockout (`winnerRound`). Its final sends the winner to the grand final.
  - The loser bracket has the rounds -2 and below (`loserRound`), -2 is its
    final.
  - Round 0 has the grand final and the reset match. The placement slots are
    in round -1 as in a knockout: index 0 is the 1st place, 1 the 2nd and 2
    the 3rd.

-}

import Bitwise
import Bracket.BestOf exposing (BestOf)
import Bracket.Knockout as Knockout exposing (Game, MatchKey, Msg, PlayerSlot)
import Dict exposing (Dict)
import PlayerId exposing (PlayerId)
import Standings exposing (Row, Standings, Status(..))


{-| With `reset`, a loss of the winner bracket winner in the grand final leads
to a second match, as both players have one loss then. Without
`finalBestOf` the grand final and the reset match are as long as the other
matches.
-}
type alias Settings =
    { reset : Bool
    , bestOf : BestOf
    , finalBestOf : Maybe BestOf
    }


defaultSettings : Settings
defaultSettings =
    { reset = True
    , bestOf = 1
    , finalBestOf = Nothing
    }


{-| A running double elimination with the settings it was started with.
-}
type alias State =
    { settings : Settings
    , game : Game
    }


{-| Player 1 comes from the winner bracket, player 2 from the loser bracket.
-}
grandFinal : MatchKey
grandFinal =
    ( 0, 0 )


{-| The winner of the grand final is player 1, the loser player 2.
-}
resetMatch : MatchKey
resetMatch =
    ( 0, 1 )


{-| The round of the winner bracket for a round of a knockout bracket.
-}
winnerRound : Int -> Int
winnerRound round =
    round + 1


isWinnerRound : Int -> Bool
isWinnerRound round =
    round > 0


{-| The round of the loser bracket that is `level` rounds before its final.
-}
loserRound : Int -> Int
loserRound level =
    -2 - level


isLoserRound : Int -> Bool
isLoserRound round =
    round <= -2


placementSlot : Int -> PlayerSlot
placementSlot index =
    ( Knockout.placementRound, index )


matchLength : Settings -> MatchKey -> BestOf
matchLength settings key =
    if key == grandFinal || key == resetMatch then
        Maybe.withDefault settings.bestOf settings.finalBestOf

    else
        settings.bestOf


start : Settings -> List PlayerId -> Game
start settings players =
    build settings <| Knockout.start players


{-| The winner bracket is seeded as in a knockout.
-}
startSeeded : Settings -> List PlayerId -> Game
startSeeded settings players =
    build settings <| Knockout.startSeeded players


{-| Where a player comes from: the winner or the loser of a match.
-}
type Feed
    = WinnerOf MatchKey
    | LoserOf MatchKey


{-| Adds the loser bracket, the grand final and the reset match to a knockout
bracket without placements.

The loser bracket is built for a winner bracket of `2 ^ k` slots, filled up
with byes. Its rounds alternate: the first round pairs the losers of the first
winner bracket round, then the survivors meet the losers of the next winner
bracket round, then the survivors play each other, and so on. A match with only
one player is left out and the player moves on directly.

-}
build : Settings -> Game -> Game
build settings knockout =
    let
        k : Int
        k =
            (Dict.keys knockout.rounds |> List.maximum |> Maybe.withDefault 0) + 1

        size : Int
        size =
            Bitwise.shiftLeftBy k 1

        shift : PlayerSlot -> PlayerSlot
        shift =
            Tuple.mapFirst winnerRound

        winners : Game
        winners =
            { knockout
                | rounds =
                    knockout.rounds
                        |> Dict.toList
                        |> List.map
                            (\( round, matches ) ->
                                ( winnerRound round
                                , Dict.map
                                    (\_ match ->
                                        { match
                                            | player1 = shift match.player1
                                            , player2 = shift match.player2
                                            , winner =
                                                Just <|
                                                    Maybe.withDefault ( 0, 0 ) <|
                                                        Maybe.map shift match.winner
                                        }
                                    )
                                    matches
                                )
                            )
                        |> Dict.fromList
                , slots =
                    knockout.slots
                        |> Dict.toList
                        |> List.map (Tuple.mapFirst shift)
                        |> Dict.fromList
            }

        -- round t of the winner bracket, counted from 1 for the first round
        winnerKey : Int -> Int -> MatchKey
        winnerKey t index =
            ( k - t + 1, index )

        firstRoundLoser : Int -> Maybe Feed
        firstRoundLoser index =
            if Knockout.matchAt (winnerKey 1 index) winners /= Nothing then
                Just <| LoserOf <| winnerKey 1 index

            else
                Nothing

        -- the inputs of the loser bracket round u, from the outputs of round u - 1
        inputs : Int -> List (Maybe Feed) -> List ( Maybe Feed, Maybe Feed )
        inputs u previous =
            if u == 1 then
                List.range 0 (size // 4 - 1)
                    |> List.map (\j -> ( firstRoundLoser (2 * j), firstRoundLoser (2 * j + 1) ))

            else if modBy 2 u == 0 then
                let
                    s =
                        u // 2

                    count =
                        List.length previous
                in
                previous
                    |> List.indexedMap
                        (\j survivor ->
                            ( survivor
                            , Just <| LoserOf <| winnerKey (s + 1) (dropIndex s count j)
                            )
                        )

            else
                pairs previous

        level : Int -> ( List (Maybe Feed), Game ) -> ( List (Maybe Feed), Game )
        level u ( previous, game ) =
            let
                round : Int
                round =
                    loserRound (2 * (k - 1) - u)
            in
            inputs u previous
                |> List.indexedMap Tuple.pair
                |> List.foldr
                    (\( j, feeds ) ( outputs, g ) ->
                        case feeds of
                            ( Just a, Just b ) ->
                                ( Just (WinnerOf ( round, j )) :: outputs
                                , g
                                    |> addMatch ( round, j )
                                    |> connect a ( round, 2 * j )
                                    |> connect b ( round, 2 * j + 1 )
                                )

                            ( Just a, Nothing ) ->
                                ( Just a :: outputs, g )

                            ( Nothing, Just b ) ->
                                ( Just b :: outputs, g )

                            ( Nothing, Nothing ) ->
                                ( Nothing :: outputs, g )
                    )
                    ( [], game )

        ( loserChampion, withLosers ) =
            if k <= 1 then
                ( Just <| LoserOf <| winnerKey 1 0, winners )

            else
                List.range 1 (2 * (k - 1))
                    |> List.foldl level ( [], winners )
                    |> Tuple.mapFirst (List.head >> Maybe.andThen identity)

        thirdPlace : Game -> Game
        thirdPlace game =
            case loserChampion of
                Just (WinnerOf key) ->
                    setLoser key (placementSlot 2) game

                _ ->
                    game

        finals : Game -> Game
        finals game =
            if settings.reset then
                game
                    |> addMatch grandFinal
                    |> setWinner grandFinal ( 0, 2 )
                    |> setLoser grandFinal ( 0, 3 )
                    |> addMatch resetMatch
                    |> setWinner resetMatch (placementSlot 0)
                    |> setLoser resetMatch (placementSlot 1)

            else
                game
                    |> addMatch grandFinal
                    |> setWinner grandFinal (placementSlot 0)
                    |> setLoser grandFinal (placementSlot 1)
    in
    if Dict.isEmpty knockout.rounds then
        knockout

    else
        withLosers
            |> finals
            |> (case loserChampion of
                    Just feed ->
                        connect feed ( 0, 1 )

                    Nothing ->
                        identity
               )
            |> thirdPlace


pairs : List a -> List ( a, a )
pairs list =
    case list of
        a :: b :: rest ->
            ( a, b ) :: pairs rest

        _ ->
            []


{-| The match of the winner bracket whose loser meets the survivor of the
loser bracket match `index` in drop round `s` (1 for the losers of the second
winner bracket round). The order is crossed, so that a dropped player doesn't
meet a player again that they just met: reversed in the odd drop rounds,
with swapped halves in the even ones.
-}
dropIndex : Int -> Int -> Int -> Int
dropIndex s count index =
    if count < 2 then
        index

    else if modBy 2 s == 0 then
        modBy count (index + count // 2)

    else
        count - 1 - index


{-| A match whose winner and loser targets are set later.
-}
addMatch : MatchKey -> Game -> Game
addMatch ( round, index ) game =
    { game
        | rounds =
            Dict.update round
                (Maybe.withDefault Dict.empty
                    >> Dict.insert index
                        { player1 = ( round, 2 * index )
                        , player2 = ( round, 2 * index + 1 )
                        , winner = Nothing
                        , loser = Nothing
                        , games = []
                        }
                    >> Just
                )
                game.rounds
    }


updateMatch : MatchKey -> (Knockout.Match -> Knockout.Match) -> Game -> Game
updateMatch ( round, index ) mapper game =
    { game
        | rounds =
            Dict.update round (Maybe.map <| Dict.update index (Maybe.map mapper)) game.rounds
    }


setWinner : MatchKey -> PlayerSlot -> Game -> Game
setWinner key slot =
    updateMatch key (\match -> { match | winner = Just slot })


setLoser : MatchKey -> PlayerSlot -> Game -> Game
setLoser key slot =
    updateMatch key (\match -> { match | loser = Just slot })


connect : Feed -> PlayerSlot -> Game -> Game
connect feed slot =
    case feed of
        WinnerOf key ->
            setWinner key slot

        LoserOf key ->
            setLoser key slot


{-| The reset match is only played if the winner of the loser bracket won the
grand final.
-}
resetNeeded : Game -> Bool
resetNeeded game =
    case ( Dict.get ( 0, 2 ) game.slots, Dict.get ( 0, 1 ) game.slots ) of
        ( Just winner, Just loserChampion ) ->
            winner == loserChampion

        _ ->
            False


{-| The tournament was decided in the grand final, so the reset match isn't
played.
-}
isResetSkipped : Game -> Bool
isResetSkipped game =
    Knockout.matchAt resetMatch game
        /= Nothing
        && Dict.member ( 0, 2 ) game.slots
        && not (resetNeeded game)


playable : MatchKey -> Game -> Bool
playable key game =
    key /= resetMatch || resetNeeded game


rules : Settings -> Knockout.Rules
rules settings =
    { matchLength = matchLength settings
    , playable = playable
    }


update : Settings -> Msg -> Game -> Game
update settings =
    Knockout.updateWith (rules settings)


hasResult : Game -> Bool
hasResult game =
    List.any (\match -> Knockout.hasResult match game) (Knockout.allMatches game)


{-| Players can only join until the first result.
-}
acceptsPlayers : Game -> Bool
acceptsPlayers game =
    not <| hasResult game


{-| The winner bracket as the knockout bracket it was built from.
-}
winnerBracket : Game -> Game
winnerBracket game =
    let
        unshift : PlayerSlot -> PlayerSlot
        unshift =
            Tuple.mapFirst (\round -> round - 1)
    in
    { game
        | rounds =
            game.rounds
                |> Dict.filter (\round _ -> isWinnerRound round)
                |> Dict.toList
                |> List.map
                    (\( round, matches ) ->
                        ( round - 1
                        , Dict.map
                            (\_ match ->
                                { match
                                    | player1 = unshift match.player1
                                    , player2 = unshift match.player2
                                    , winner =
                                        match.winner
                                            |> Maybe.map unshift
                                            |> Maybe.andThen
                                                (\( r, i ) ->
                                                    if r < 0 then
                                                        Nothing

                                                    else
                                                        Just ( r, i )
                                                )
                                    , loser = Nothing
                                }
                            )
                            matches
                        )
                    )
                |> Dict.fromList
        , slots =
            game.slots
                |> Dict.filter (\( round, _ ) _ -> isWinnerRound round)
                |> Dict.toList
                |> List.map (Tuple.mapFirst unshift)
                |> Dict.fromList
    }


{-| The new player gets a place in the winner bracket as in a knockout, and the
loser bracket grows with it. After the first result the tournament stays as it
is.
-}
addPlayer : Settings -> PlayerId -> Game -> Game
addPlayer settings id game =
    if acceptsPlayers game then
        winnerBracket game
            |> Knockout.addPlayer id
            |> build settings
            |> Knockout.resolveRemovedWhere playable

    else
        game


{-| The opponents of the player win by walkover, in both brackets.
-}
removePlayer : PlayerId -> Game -> Game
removePlayer id game =
    Knockout.resolveRemovedWhere playable
        { game
            | players =
                Dict.update id
                    (Maybe.map <| \player -> { player | alive = False, removed = True })
                    game.players
        }


isEliminated : PlayerId -> Game -> Bool
isEliminated =
    Knockout.isEliminated


isRemoved : PlayerId -> Game -> Bool
isRemoved =
    Knockout.isRemoved


{-| 1st and 2nd from the grand final or the reset match, 3rd is the loser of
the loser bracket final.
-}
topPlaces : Game -> Dict PlayerId Int
topPlaces game =
    let
        at : PlayerSlot -> List PlayerId
        at slot =
            Dict.get slot game.slots
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        ( first, second ) =
            if isResetSkipped game then
                ( at ( 0, 2 )
                , at ( 0, 3 ) |> List.filter (\id -> not <| Knockout.isRemoved id game)
                )

            else
                ( at (placementSlot 0), at (placementSlot 1) )
    in
    [ ( 1, first ), ( 2, second ), ( 3, at (placementSlot 2) ) ]
        |> List.concatMap (\( place, ids ) -> List.map (\id -> ( id, place )) ids)
        |> Dict.fromList


{-| Places 1 to 3 as in `topPlaces`. The losers of a loser bracket round share
the place after all players who got further. A player who was removed and lost
by walkover gets no place.
-}
toStandings : Settings -> Game -> Standings
toStandings settings game =
    let
        allResults : List Knockout.MatchResult
        allResults =
            Knockout.results (matchLength settings) game

        k : Int
        k =
            Dict.keys game.rounds |> List.filter isWinnerRound |> List.maximum |> Maybe.withDefault 0

        matchesIn : Int -> Int
        matchesIn round =
            Dict.get round game.rounds |> Maybe.map Dict.size |> Maybe.withDefault 0

        lastLoss : Dict PlayerId Knockout.MatchResult
        lastLoss =
            allResults
                |> List.sortBy (.key >> matchOrder)
                |> List.map (\result -> ( result.loser, result ))
                |> Dict.fromList

        -- a player plays the winner bracket, then the loser bracket, then the finals
        matchOrder : MatchKey -> ( Int, Int )
        matchOrder ( round, index ) =
            if isWinnerRound round then
                ( 0, 0 )

            else if isLoserRound round then
                ( 1, round )

            else
                ( 2, index )

        slotPlace : Dict PlayerId Int
        slotPlace =
            topPlaces game

        derivedPlace : PlayerId -> Maybe Int
        derivedPlace id =
            Dict.get id lastLoss
                |> Maybe.andThen
                    (\{ key, walkover } ->
                        let
                            round =
                                Tuple.first key
                        in
                        if walkover && Knockout.isRemoved id game then
                            Nothing

                        else if isLoserRound round then
                            List.range (round + 1) -2
                                |> List.map matchesIn
                                |> List.sum
                                |> (+) 3
                                |> Just

                        else
                            Nothing
                    )

        placeOfPlayer : Knockout.Player -> Maybe Int
        placeOfPlayer player =
            case Dict.get player.id slotPlace of
                Just place ->
                    Just place

                Nothing ->
                    if player.alive then
                        Nothing

                    else
                        derivedPlace player.id

        matchName : MatchKey -> String
        matchName (( round, _ ) as key) =
            if isWinnerRound round then
                "Winner bracket round " ++ String.fromInt (k - round + 1)

            else if isLoserRound round then
                "Loser bracket round " ++ String.fromInt (2 * k + round)

            else if key == grandFinal then
                "Grand final"

            else
                "Reset match"

        withGames : Bool
        withGames =
            settings.bestOf > 1 || Maybe.withDefault 1 settings.finalBestOf > 1

        toRow : Knockout.Player -> Row
        toRow player =
            let
                base : Row
                base =
                    Standings.row
                        { id = player.id
                        , place = placeOfPlayer player
                        , alive = player.alive
                        , removed = player.removed
                        }

                won : List Knockout.MatchResult
                won =
                    List.filter (\result -> result.winner == player.id) allResults

                lost : List Knockout.MatchResult
                lost =
                    List.filter (\result -> result.loser == player.id) allResults

                ( gamesWon, gamesLost ) =
                    List.map .games won
                        ++ List.map (.games >> (\( a, b ) -> ( b, a ))) lost
                        |> List.foldl (\( a, b ) ( sumA, sumB ) -> ( sumA + a, sumB + b )) ( 0, 0 )

                isOut : Bool
                isOut =
                    case base.place of
                        Just place ->
                            place > 1

                        Nothing ->
                            base.status /= StillIn
            in
            { base
                | won = List.length won
                , lost = List.length lost
                , extra =
                    (if isOut then
                        Dict.get player.id lastLoss
                            |> Maybe.map (.key >> matchName)
                            |> Maybe.withDefault ""

                     else
                        ""
                    )
                        :: (if withGames then
                                [ String.fromInt gamesWon ++ " : " ++ String.fromInt gamesLost ]

                            else
                                []
                           )
            }

        -- players with fewer losses and more wins first
        progress : PlayerId -> Int
        progress id =
            List.foldl
                (\result sum ->
                    if result.loser == id then
                        sum + 100

                    else if result.winner == id then
                        sum - 1

                    else
                        sum
                )
                0
                allResults

        rows : List Row
        rows =
            Knockout.participants game
                |> List.filterMap (\id -> Dict.get id game.players)
                |> List.map toRow
                |> Standings.order progress
    in
    { rows = rows
    , finished = not (List.isEmpty rows) && List.all (.status >> (/=) StillIn) rows
    , liveRanking = False
    , draws = False
    , columns =
        "Out in"
            :: (if withGames then
                    [ "Games" ]

                else
                    []
               )
    }
