module Bracket.SwissTest exposing (suite)

import Bracket.Scoring exposing (Cell(..), Scoring(..))
import Bracket.Swiss as Swiss exposing (Game, Msg(..), Round, State)
import Bracket.Swiss.Serializer as Serializer
import Dict
import Expect exposing (Expectation)
import Json.Decode as JD
import PlayerId exposing (PlayerId)
import Set
import Standings
import Test exposing (Test, describe, test)
import Tournament exposing (Tournament(..))


start : Scoring -> Maybe Int -> Int -> State
start scoring rounds n =
    Swiss.start { rounds = rounds, scoring = scoring } (List.range 0 (n - 1))


{-| Enters the results of the current round. The players can be given in any
order; the scores are in the same order.
-}
results : List ( PlayerId, PlayerId, ( Int, Int ) ) -> State -> State
results list state =
    List.foldl
        (\( a, b, ( scoreA, scoreB ) ) s ->
            if Swiss.isEditable b a s.game then
                Swiss.update (SetResult b a ( scoreB, scoreA )) s

            else
                Swiss.update (SetResult a b ( scoreA, scoreB )) s
        )
        state
        list


pairRound : (PlayerId -> PlayerId -> Bool) -> List PlayerId -> List PlayerId -> Maybe ( List ( PlayerId, PlayerId ), Maybe PlayerId )
pairRound allowed hadBye ranked =
    Swiss.pairRound { allowed = allowed, hadBye = \id -> List.member id hadBye } ranked
        |> Maybe.map (\round -> ( pairs round, round.bye ))


except : List ( PlayerId, PlayerId ) -> PlayerId -> PlayerId -> Bool
except blocked a b =
    not <| List.member ( min a b, max a b ) blocked


nextRound : State -> State
nextRound =
    Swiss.update NextRound


pairs : Round -> List ( PlayerId, PlayerId )
pairs round =
    List.map (\match -> ( match.home, match.away )) round.matches


current : State -> Maybe Round
current state =
    Swiss.currentRound state.game


places : State -> List ( PlayerId, Maybe Int )
places state =
    Swiss.standings state
        |> List.map (\standing -> ( standing.id, standing.place ))


{-| The lower id wins, so the ranking is never tied for long.
-}
lowerWins : State -> State
lowerWins state =
    current state
        |> Maybe.map .matches
        |> Maybe.withDefault []
        |> List.foldl
            (\match ->
                Swiss.update <|
                    SetResult match.home match.away <|
                        if match.home < match.away then
                            ( 2, 1 )

                        else
                            ( 1, 2 )
            )
            state


{-| Plays every round with `lowerWins` until the tournament is finished.
-}
playOut : State -> State
playOut state =
    let
        played =
            lowerWins state
    in
    if Swiss.isFinished played.game || List.length played.game.rounds > 20 then
        played

    else
        playOut <| nextRound played


{-| The players in the rounds after the first `skip` rounds.
-}
playersFrom : Int -> State -> List PlayerId
playersFrom skip state =
    state.game.rounds
        |> List.drop skip
        |> List.concatMap (\round -> List.concatMap (\( a, b ) -> [ a, b ]) (pairs round) ++ List.filterMap identity [ round.bye ])


{-| Every round has every active player exactly once, and no pair plays twice.
-}
expectValidRounds : State -> Expectation
expectValidRounds state =
    let
        allPairs =
            List.concatMap pairs state.game.rounds
                |> List.map (\( a, b ) -> ( min a b, max a b ))

        inRound round =
            List.concatMap (\( a, b ) -> [ a, b ]) (pairs round)
                ++ List.filterMap identity [ round.bye ]
    in
    Expect.all
        [ \_ ->
            List.length allPairs
                |> Expect.equal (Set.size <| Set.fromList allPairs)
                |> Expect.onFail "a pair played twice"
        , \_ ->
            state.game.rounds
                |> List.map (inRound >> List.sort)
                |> List.all (\ids -> Set.size (Set.fromList ids) == List.length ids)
                |> Expect.equal True
                |> Expect.onFail "a player is twice in a round"
        ]
        ()


{-| Four players: 0 beats 2 and draws with 1, 1 beats 3, 2 beats 3.
-}
fourPlayers : Scoring -> State
fourPlayers scoring =
    start scoring Nothing 4
        |> results [ ( 0, 2, ( 1, 0 ) ), ( 1, 3, ( 1, 0 ) ) ]
        |> nextRound
        |> results [ ( 0, 1, ( 1, 1 ) ), ( 2, 3, ( 2, 0 ) ) ]


suite : Test
suite =
    describe "Bracket.Swiss"
        [ describe "start"
            [ test "the automatic rounds are the rounded up log2 of the players" <|
                \_ ->
                    List.map Swiss.autoRounds [ 2, 3, 4, 5, 7, 8, 9, 16, 17 ]
                        |> Expect.equal [ 1, 2, 2, 3, 3, 3, 4, 4, 5 ]
            , test "the automatic rounds are fixed at the start" <|
                \_ ->
                    start Points Nothing 7
                        |> .game
                        |> .totalRounds
                        |> Expect.equal 3
            , test "more rounds than players - 1 are cut" <|
                \_ ->
                    List.map (\rounds -> (start Points (Just rounds) 4).game.totalRounds) [ 0, 1, 3, 4, 9 ]
                        |> Expect.equal [ 1, 1, 3, 3, 3 ]
            , test "2 players play 1 round" <|
                \_ ->
                    (start Points Nothing 2).game.totalRounds
                        |> Expect.equal 1
            , test "the top half plays against the bottom half" <|
                \_ ->
                    start Points Nothing 6
                        |> current
                        |> Expect.equal (Just { matches = [ { home = 0, away = 3, result = Nothing }, { home = 1, away = 4, result = Nothing }, { home = 2, away = 5, result = Nothing } ], bye = Nothing })
            , test "with an odd number the last player has a bye" <|
                \_ ->
                    start Points Nothing 7
                        |> current
                        |> Maybe.map (\round -> ( pairs round, round.bye ))
                        |> Expect.equal (Just ( [ ( 0, 3 ), ( 1, 4 ), ( 2, 5 ) ], Just 6 ))
            , test "the start order is the list order" <|
                \_ ->
                    Swiss.start Swiss.defaultSettings [ 5, 2, 9, 1 ]
                        |> current
                        |> Maybe.map pairs
                        |> Expect.equal (Just [ ( 5, 9 ), ( 2, 1 ) ])
            ]
        , describe "results"
            [ test "a result is stored in the match" <|
                \_ ->
                    start Points Nothing 4
                        |> results [ ( 0, 2, ( 3, 1 ) ) ]
                        |> current
                        |> Maybe.andThen (.matches >> List.head)
                        |> Maybe.andThen .result
                        |> Expect.equal (Just ( 3, 1 ))
            , test "winner only stores 1:0, 0:0 or 0:1" <|
                \_ ->
                    start WinnerOnly Nothing 4
                        |> results [ ( 0, 2, ( 3, 1 ) ), ( 1, 3, ( 2, 2 ) ) ]
                        |> current
                        |> Maybe.map (.matches >> List.map .result)
                        |> Expect.equal (Just [ Just ( 1, 0 ), Just ( 0, 0 ) ])
            , test "a result can be changed and cleared" <|
                \_ ->
                    start Points Nothing 4
                        |> results [ ( 0, 2, ( 3, 1 ) ), ( 0, 2, ( 0, 1 ) ), ( 1, 3, ( 1, 1 ) ) ]
                        |> Swiss.update (ClearResult 1 3)
                        |> current
                        |> Maybe.map (.matches >> List.map .result)
                        |> Expect.equal (Just [ Just ( 0, 1 ), Nothing ])
            , test "a pair that doesn't play in the current round is ignored" <|
                \_ ->
                    let
                        state =
                            start Points Nothing 4
                    in
                    state
                        |> Swiss.update (SetResult 2 0 ( 3, 1 ))
                        |> Swiss.update (SetResult 0 1 ( 3, 1 ))
                        |> Expect.equal state
            , test "results of an earlier round can't be changed" <|
                \_ ->
                    let
                        state =
                            start Points Nothing 4
                                |> results [ ( 0, 2, ( 1, 0 ) ), ( 1, 3, ( 1, 0 ) ) ]
                                |> nextRound
                    in
                    state
                        |> results [ ( 0, 2, ( 0, 5 ) ) ]
                        |> Swiss.update (ClearResult 1 3)
                        |> Expect.equal state
            ]
        , describe "next round"
            [ test "waits until every match of the round has a result" <|
                \_ ->
                    let
                        state =
                            start Points Nothing 4
                                |> results [ ( 0, 2, ( 1, 0 ) ) ]
                    in
                    nextRound state
                        |> Expect.equal state
            , test "pairs the players by their ranking" <|
                \_ ->
                    start Points Nothing 4
                        |> results [ ( 0, 2, ( 0, 1 ) ), ( 1, 3, ( 1, 0 ) ) ]
                        |> nextRound
                        |> current
                        |> Maybe.map pairs
                        |> Expect.equal (Just [ ( 1, 2 ), ( 0, 3 ) ])
            , test "never repeats a pair" <|
                \_ ->
                    start Points Nothing 4
                        |> results [ ( 0, 2, ( 1, 0 ) ), ( 1, 3, ( 1, 0 ) ) ]
                        |> nextRound
                        |> results [ ( 0, 1, ( 1, 0 ) ), ( 2, 3, ( 1, 0 ) ) ]
                        |> (\state -> { state | game = (\game -> { game | totalRounds = 3 }) state.game })
                        |> nextRound
                        |> current
                        |> Maybe.map pairs
                        |> Expect.equal (Just [ ( 0, 3 ), ( 1, 2 ) ])
            , test "the lowest-ranked player without a bye gets the bye" <|
                \_ ->
                    start Points Nothing 5
                        |> results [ ( 0, 2, ( 0, 1 ) ), ( 1, 3, ( 1, 0 ) ) ]
                        |> nextRound
                        |> current
                        |> Maybe.map (\round -> ( round.bye, pairs round ))
                        |> Expect.equal (Just ( Just 3, [ ( 1, 2 ), ( 4, 0 ) ] ))
            , test "a bye is a win without points" <|
                \_ ->
                    start Points Nothing 3
                        |> Swiss.standings
                        |> List.filter (.id >> (==) 2)
                        |> List.map (\s -> ( ( s.wins, s.matchPoints ), ( s.scored, s.conceded ) ))
                        |> Expect.equal [ ( ( 1, 3 ), ( 0, 0 ) ) ]
            , test "after the last round the tournament is finished" <|
                \_ ->
                    let
                        state =
                            start Points (Just 1) 4
                                |> results [ ( 0, 2, ( 1, 0 ) ), ( 1, 3, ( 1, 0 ) ) ]
                    in
                    ( Swiss.isFinished state.game, nextRound state == state )
                        |> Expect.equal ( True, True )
            , test "finishes early if nobody can be paired anymore" <|
                \_ ->
                    let
                        round list =
                            { matches = List.map (\( a, b ) -> { home = a, away = b, result = Just ( 1, 0 ) }) list
                            , bye = Nothing
                            }

                        state =
                            { settings = Swiss.defaultSettings
                            , game =
                                { players = List.map (\id -> { id = id, removed = False }) [ 0, 1, 2, 3 ]
                                , totalRounds = 5
                                , rounds = [ round [ ( 0, 1 ), ( 2, 3 ) ], round [ ( 0, 2 ), ( 1, 3 ) ], round [ ( 0, 3 ), ( 1, 2 ) ] ]
                                }
                            }
                    in
                    nextRound state
                        |> .game
                        |> (\game -> ( game.totalRounds, List.length game.rounds, Swiss.isFinished game ))
                        |> Expect.equal ( 3, 3, True )
            , describe "full tournaments keep every player once per round and repeat no pair" <|
                List.map
                    (\n ->
                        test (String.fromInt n ++ " players") <|
                            \_ ->
                                start Points (Just (n - 1)) n
                                    |> playOut
                                    |> Expect.all
                                        [ expectValidRounds
                                        , .game >> Swiss.isFinished >> Expect.equal True
                                        ]
                    )
                    (List.range 2 12)
            , test "with an odd number every player has at most one bye" <|
                \_ ->
                    start Points (Just 6) 7
                        |> playOut
                        |> .game
                        |> .rounds
                        |> List.filterMap .bye
                        |> (\byes -> List.length byes == Set.size (Set.fromList byes))
                        |> Expect.equal True
            ]
        , describe "pairing"
            [ test "pairs from the top" <|
                \_ ->
                    pairRound (\_ _ -> True) [] [ 3, 1, 2, 0 ]
                        |> Expect.equal (Just ( [ ( 3, 1 ), ( 2, 0 ) ], Nothing ))
            , test "backtracks if the rest can't be paired" <|
                \_ ->
                    pairRound (except [ ( 2, 3 ), ( 2, 4 ), ( 2, 5 ) ]) [] (List.range 0 5)
                        |> Expect.equal (Just ( [ ( 0, 2 ), ( 1, 3 ), ( 4, 5 ) ], Nothing ))
            , test "backtracks over several levels" <|
                \_ ->
                    pairRound (except [ ( 4, 6 ), ( 4, 7 ), ( 5, 6 ), ( 5, 7 ), ( 6, 7 ) ]) [] (List.range 0 7)
                        |> Expect.equal (Just ( [ ( 0, 1 ), ( 2, 6 ), ( 3, 7 ), ( 4, 5 ) ], Nothing ))
            , test "fails if no pairing exists" <|
                \_ ->
                    pairRound (\a b -> a == 0 || b == 0) [] [ 0, 1, 2, 3 ]
                        |> Expect.equal Nothing
            , test "fails with fewer than 2 players" <|
                \_ ->
                    List.map (pairRound (\_ _ -> True) []) [ [], [ 0 ] ]
                        |> Expect.equal [ Nothing, Nothing ]
            , test "the lowest-ranked player without a bye gets it" <|
                \_ ->
                    pairRound (\_ _ -> True) [ 4, 3 ] (List.range 0 4)
                        |> Expect.equal (Just ( [ ( 0, 1 ), ( 3, 4 ) ], Just 2 ))
            , test "if everyone had a bye, the lowest-ranked player gets it" <|
                \_ ->
                    pairRound (\_ _ -> True) (List.range 0 2) (List.range 0 2)
                        |> Expect.equal (Just ( [ ( 0, 1 ) ], Just 2 ))
            , test "the bye moves up if the others can't be paired otherwise" <|
                \_ ->
                    pairRound (except [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]) [] (List.range 0 4)
                        |> Expect.equal (Just ( [ ( 0, 4 ), ( 1, 2 ) ], Just 3 ))
            , test "a late round with few pairs left" <|
                \_ ->
                    let
                        round list =
                            { matches = List.map (\( a, b ) -> { home = a, away = b, result = Just ( 0, 0 ) }) list
                            , bye = Nothing
                            }

                        -- only draws, so all players are tied and keep the start order
                        state =
                            { settings = Swiss.defaultSettings
                            , game =
                                { players = List.map (\id -> { id = id, removed = False }) (List.range 0 5)
                                , totalRounds = 5
                                , rounds =
                                    [ round [ ( 0, 3 ), ( 1, 4 ), ( 2, 5 ) ]
                                    , round [ ( 0, 4 ), ( 1, 5 ), ( 2, 3 ) ]
                                    , round [ ( 0, 5 ), ( 1, 2 ), ( 3, 4 ) ]
                                    ]
                                }
                            }
                    in
                    nextRound state
                        |> Expect.all
                            [ current >> Maybe.map pairs >> Expect.equal (Just [ ( 0, 1 ), ( 2, 4 ), ( 3, 5 ) ])
                            , expectValidRounds
                            ]
            ]
        , describe "ranking"
            [ test "match points decide first" <|
                \_ ->
                    fourPlayers Points
                        |> places
                        |> List.map Tuple.first
                        |> List.take 2
                        |> Expect.equal [ 0, 1 ]
            , test "Buchholz decides equal match points" <|
                \_ ->
                    fourPlayers Points
                        |> Swiss.standings
                        |> List.map (\s -> ( s.id, s.matchPoints, s.buchholz ))
                        |> Expect.equal [ ( 0, 4, 7 ), ( 1, 4, 4 ), ( 2, 3, 4 ), ( 3, 0, 7 ) ]
            , test "the score difference decides equal Buchholz with points" <|
                \_ ->
                    start Points Nothing 4
                        |> results [ ( 0, 2, ( 1, 0 ) ), ( 1, 3, ( 5, 0 ) ) ]
                        |> nextRound
                        |> results [ ( 0, 1, ( 1, 0 ) ), ( 2, 3, ( 1, 0 ) ) ]
                        |> places
                        |> Expect.equal [ ( 0, Just 1 ), ( 1, Just 2 ), ( 2, Just 3 ), ( 3, Just 4 ) ]
            , test "without points equal Buchholz is a shared place" <|
                \_ ->
                    start WinnerOnly Nothing 4
                        |> results [ ( 0, 2, ( 1, 0 ) ), ( 1, 3, ( 5, 0 ) ) ]
                        |> nextRound
                        |> results [ ( 0, 1, ( 1, 0 ) ), ( 2, 3, ( 1, 0 ) ) ]
                        |> places
                        |> Expect.equal [ ( 0, Just 1 ), ( 1, Just 2 ), ( 2, Just 2 ), ( 3, Just 4 ) ]
            , test "the podium waits until the tournament is finished" <|
                \_ ->
                    start Points Nothing 4
                        |> results [ ( 0, 2, ( 1, 0 ) ), ( 1, 3, ( 1, 0 ) ) ]
                        |> nextRound
                        |> results [ ( 0, 1, ( 1, 0 ) ) ]
                        |> Swiss.toStandings
                        |> Standings.podium
                        |> Expect.equal []
            , test "the podium once finished" <|
                \_ ->
                    fourPlayers Points
                        |> Swiss.toStandings
                        |> Standings.podium
                        |> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 1 ] ), ( 3, [ 2 ] ) ]
            ]
        , describe "players"
            [ test "an open match of a removed player is a forfeit win for the opponent" <|
                \_ ->
                    start Points Nothing 4
                        |> Swiss.removePlayer 2
                        |> (\state ->
                                current state
                                    |> Maybe.map (.matches >> List.map (Swiss.matchCell state.game))
                           )
                        |> Expect.equal (Just [ Forfeit True, Open ])
            , test "a removed player isn't paired again and has no place" <|
                \_ ->
                    start Points Nothing 4
                        |> results [ ( 1, 3, ( 1, 0 ) ) ]
                        |> Swiss.removePlayer 2
                        |> nextRound
                        |> Expect.all
                            [ current >> Maybe.map (\round -> ( pairs round, round.bye )) >> Expect.equal (Just ( [ ( 1, 0 ) ], Just 3 ))
                            , places >> Expect.equal [ ( 1, Just 1 ), ( 0, Just 2 ), ( 3, Just 3 ), ( 2, Nothing ) ]
                            ]
            , test "a match of a removed player can't be changed" <|
                \_ ->
                    let
                        state =
                            start Points Nothing 4
                                |> results [ ( 0, 2, ( 1, 0 ) ) ]
                                |> Swiss.removePlayer 2
                    in
                    state
                        |> results [ ( 0, 2, ( 0, 1 ) ) ]
                        |> Swiss.update (ClearResult 0 2)
                        |> Expect.equal state
            , test "an added player starts with 0 points and plays from the next round" <|
                \_ ->
                    start Points Nothing 4
                        |> Swiss.addPlayer 7
                        |> Expect.all
                            [ current >> Maybe.map pairs >> Expect.equal (Just [ ( 0, 2 ), ( 1, 3 ) ])
                            , Swiss.standings
                                >> List.filter (.id >> (==) 7)
                                >> List.map (\s -> ( s.matchPoints, s.wins + s.draws + s.losses ))
                                >> Expect.equal [ ( 0, 0 ) ]
                            , results [ ( 0, 2, ( 1, 0 ) ), ( 1, 3, ( 1, 0 ) ) ]
                                >> nextRound
                                >> current
                                >> Maybe.map (\round -> ( pairs round, round.bye ))
                                >> Expect.equal (Just ( [ ( 0, 1 ), ( 2, 3 ) ], Just 7 ))
                            ]
            , test "adding a player twice is ignored" <|
                \_ ->
                    start Points Nothing 4
                        |> Swiss.addPlayer 2
                        |> Expect.equal (start Points Nothing 4)
            , test "a tournament with a removal and an addition stays valid" <|
                \_ ->
                    start Points (Just 6) 7
                        |> lowerWins
                        |> nextRound
                        |> Swiss.removePlayer 3
                        |> Swiss.addPlayer 7
                        |> lowerWins
                        |> nextRound
                        |> Swiss.addPlayer 8
                        |> Swiss.removePlayer 0
                        |> playOut
                        |> Expect.all
                            [ expectValidRounds
                            , .game >> Swiss.isFinished >> Expect.equal True
                            , \state ->
                                ( playersFrom 2 state |> List.member 3, playersFrom 3 state |> List.member 0 )
                                    |> Expect.equal ( False, False )
                                    |> Expect.onFail "a removed player was paired again"
                            , playersFrom 2 >> List.member 7 >> Expect.equal True
                            , playersFrom 3 >> List.member 8 >> Expect.equal True
                            ]
            ]
        , test "the serializer keeps the state" <|
            \_ ->
                let
                    state =
                        start WinnerOnly (Just 3) 5
                            |> results [ ( 0, 2, ( 1, 0 ) ) ]
                            |> Swiss.removePlayer 3

                    roundTrip encode decoder value =
                        JD.decodeValue decoder (encode value)
                in
                ( roundTrip Serializer.encodeSettings Serializer.decodeSettings state.settings
                , roundTrip Serializer.encodeGame Serializer.decodeGame state.game
                )
                    |> Expect.equal ( Ok state.settings, Ok state.game )
        , test "the CSV has the standings" <|
            \_ ->
                Tournament.csv (Swiss <| fourPlayers Points)
                    |> Maybe.map (\toCsv -> toCsv (Dict.fromList [ ( 0, "A" ), ( 1, "B" ), ( 2, "C" ), ( 3, "D;x" ) ]))
                    |> Expect.equal
                        (Just <|
                            "\u{FEFF}"
                                ++ String.concat
                                    (List.map (\line -> line ++ "\u{000D}\n")
                                        [ "Place;Name;W;D;L;Score;Pts;Buchholz"
                                        , "1;A;1;1;0;2 : 1;4;7"
                                        , "2;B;1;1;0;2 : 1;4;4"
                                        , "3;C;1;0;1;2 : 1;3;4"
                                        , "4;\"D;x\";0;0;2;0 : 3;0;7"
                                        ]
                                    )
                        )
        ]
