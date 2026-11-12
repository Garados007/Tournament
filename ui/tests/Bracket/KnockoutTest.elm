module Bracket.KnockoutTest exposing (suite)

import Bracket.Knockout exposing (Game, generateTournament, hideFinishedMatches, initialSlots)
import Dict
import Expect
import Helper exposing (addPlayer, allMatches, expectValidStructure, inputSlots, matchAt, matchesPerRound, players, setWinner, start, startWithPlacement)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Bracket.Knockout"
        [ generateTournamentTests
        , setInitialPlayerTests
        , updateTests
        , addPositionTests
        , hideFinishedMatchesTests
        ]


{-| Number of matches per round, starting with the final round 0. The last
round of a player count that isn't a power of two is the pre-round.
-}
expectedMatches : List ( Int, List Int )
expectedMatches =
    [ ( 2, [ 1 ] )
    , ( 3, [ 1, 1 ] )
    , ( 4, [ 2, 2 ] )
    , ( 5, [ 2, 2, 1 ] )
    , ( 6, [ 2, 2, 2 ] )
    , ( 7, [ 2, 2, 3 ] )
    , ( 8, [ 2, 2, 4 ] )
    , ( 9, [ 2, 2, 4, 1 ] )
    , ( 10, [ 2, 2, 4, 2 ] )
    , ( 11, [ 2, 2, 4, 3 ] )
    , ( 12, [ 2, 2, 4, 4 ] )
    , ( 13, [ 2, 2, 4, 5 ] )
    , ( 14, [ 2, 2, 4, 6 ] )
    , ( 15, [ 2, 2, 4, 7 ] )
    , ( 16, [ 2, 2, 4, 8 ] )
    , ( 17, [ 2, 2, 4, 8, 1 ] )
    ]


playerCounts : List Int
playerCounts =
    List.range 2 17


forEachCount : String -> (Int -> Expect.Expectation) -> Test
forEachCount name check =
    describe name <|
        List.map
            (\n -> test (String.fromInt n ++ " players") <| \_ -> check n)
            playerCounts


generateTournamentTests : Test
generateTournamentTests =
    describe "generateTournament"
        [ describe "matches per round" <|
            List.map
                (\( n, matches ) ->
                    test (String.fromInt n ++ " players") <|
                        \_ ->
                            generateTournament n
                                |> matchesPerRound
                                |> Expect.equal matches
                )
                expectedMatches
        , forEachCount "rounds are numbered from 0" <|
            \n ->
                generateTournament n
                    |> .rounds
                    |> Dict.keys
                    |> Expect.equal (List.range 0 (List.length (matchesPerRound (generateTournament n)) - 1))
        , forEachCount "has one initial slot per player" <|
            \n ->
                generateTournament n
                    |> initialSlots
                    |> List.length
                    |> Expect.equal n
        , forEachCount "pre-round only for non-powers of two" <|
            \n ->
                let
                    game =
                        generateTournament n

                    lastRound =
                        Dict.size game.rounds - 1

                    isPowerOfTwo =
                        List.member n [ 2, 4, 8, 16 ]

                    lastRoundInputs =
                        Dict.get lastRound game.rounds
                            |> Maybe.map Dict.size
                            |> Maybe.withDefault 0
                            |> (*) 2
                in
                if isPowerOfTwo then
                    Expect.equal n lastRoundInputs

                else
                    -- some players skip the pre-round and start one round later
                    Expect.lessThan n lastRoundInputs
        , forEachCount "winner and loser slots point to existing match inputs" <|
            \n -> expectValidStructure (generateTournament n)
        , forEachCount "the final has no winner or loser slot" <|
            \n ->
                matchAt ( 0, 0 ) (generateTournament n)
                    |> Maybe.map (\match -> ( match.winner, match.loser ))
                    |> Expect.equal (Just ( Nothing, Nothing ))
        , forEachCount "third-place match exists only for 4 or more players" <|
            \n ->
                matchAt ( 0, 1 ) (generateTournament n)
                    |> Expect.equal
                        (if n >= 4 then
                            Just
                                { player1 = ( 0, 2 )
                                , player2 = ( 0, 3 )
                                , winner = Nothing
                                , loser = Nothing
                                }

                         else
                            Nothing
                        )
        , forEachCount "semi-final losers go to the third-place match" <|
            \n ->
                [ matchAt ( 1, 0 ), matchAt ( 1, 1 ) ]
                    |> List.map (\get -> get (generateTournament n) |> Maybe.andThen .loser)
                    |> Expect.equal
                        (if n >= 4 then
                            [ Just ( 0, 2 ), Just ( 0, 3 ) ]

                         else
                            [ Nothing, Nothing ]
                        )
        , test "3 players: one player waits for the final" <|
            \_ ->
                generateTournament 3
                    |> .rounds
                    |> Expect.equal
                        (Dict.fromList
                            [ ( 0
                              , Dict.fromList
                                    [ ( 0, { player1 = ( 0, 0 ), player2 = ( 1, 2 ), winner = Nothing, loser = Nothing } ) ]
                              )
                            , ( 1
                              , Dict.fromList
                                    [ ( 0, { player1 = ( 1, 0 ), player2 = ( 1, 1 ), winner = Just ( 0, 0 ), loser = Nothing } ) ]
                              )
                            ]
                        )
        ]


setInitialPlayerTests : Test
setInitialPlayerTests =
    describe "setInitialPlayers"
        [ forEachCount "puts the players into the initial slots in list order" <|
            \n ->
                start n
                    |> .slots
                    |> Expect.equal
                        (List.map2 (\slot ( id, _ ) -> ( slot, id ))
                            (initialSlots (generateTournament n))
                            (players n)
                            |> Dict.fromList
                        )
        , forEachCount "every player occupies exactly one slot" <|
            \n ->
                start n
                    |> .slots
                    |> Dict.values
                    |> List.sort
                    |> Expect.equal (List.range 0 (n - 1))
        , forEachCount "every slot is an input of a match" <|
            \n ->
                let
                    game =
                        start n
                in
                Dict.keys game.slots
                    |> List.filter (\slot -> not <| List.member slot (inputSlots game))
                    |> Expect.equal []
        , test "registers all players as alive" <|
            \_ ->
                start 3
                    |> .players
                    |> Dict.values
                    |> Expect.equal
                        [ { id = 0, name = "P0", alive = True }
                        , { id = 1, name = "P1", alive = True }
                        , { id = 2, name = "P2", alive = True }
                        ]
        ]


aliveOf : Int -> Game Int -> Maybe Bool
aliveOf id game =
    Dict.get id game.players |> Maybe.map .alive


updateTests : Test
updateTests =
    describe "update SetWinner"
        [ test "the winner moves on and the loser is out" <|
            \_ ->
                let
                    -- 8 players: quarter-final (2, 0) is P0 against P1
                    game =
                        startWithPlacement 8 |> setWinner True ( 2, 0 )
                in
                Expect.all
                    [ \g -> Dict.get ( 1, 0 ) g.slots |> Expect.equal (Just 1)
                    , \g -> aliveOf 0 g |> Expect.equal (Just False)
                    , \g -> aliveOf 1 g |> Expect.equal (Just True)
                    , \g -> Dict.get ( 2, 0 ) g.slots |> Expect.equal (Just 0)
                    ]
                    game
        , test "a semi-final loser moves to the third-place match and stays alive" <|
            \_ ->
                startWithPlacement 4
                    |> setWinner False ( 1, 0 )
                    |> setWinner True ( 1, 1 )
                    |> Expect.all
                        [ \g -> matchAt ( 0, 0 ) g |> Maybe.map (\p -> ( Dict.get p.player1 g.slots, Dict.get p.player2 g.slots )) |> Expect.equal (Just ( Just 0, Just 3 ))
                        , \g -> matchAt ( 0, 1 ) g |> Maybe.map (\p -> ( Dict.get p.player1 g.slots, Dict.get p.player2 g.slots )) |> Expect.equal (Just ( Just 1, Just 2 ))
                        , \g -> List.map (\id -> aliveOf id g) [ 0, 1, 2, 3 ] |> Expect.equal (List.repeat 4 (Just True))
                        ]
        , test "the final and the third-place match fill the placement slots" <|
            \_ ->
                startWithPlacement 4
                    |> setWinner False ( 1, 0 )
                    |> setWinner True ( 1, 1 )
                    |> setWinner True ( 0, 0 )
                    |> setWinner False ( 0, 1 )
                    |> Expect.all
                        [ \g ->
                            List.map (\slot -> Dict.get slot g.slots) [ ( -1, 0 ), ( -1, 1 ), ( -1, 2 ) ]
                                |> Expect.equal [ Just 3, Just 0, Just 1 ]
                        , -- the final has a loser slot, so its loser stays alive
                          \g -> List.map (\id -> aliveOf id g) [ 0, 1, 2, 3 ] |> Expect.equal [ Just True, Just True, Just False, Just True ]
                        ]
        , test "without placement slots the final only marks the loser" <|
            \_ ->
                start 2
                    |> setWinner False ( 0, 0 )
                    |> Expect.all
                        [ \g -> Dict.toList g.slots |> Expect.equal [ ( ( 0, 0 ), 0 ), ( ( 0, 1 ), 1 ) ]
                        , \g -> aliveOf 1 g |> Expect.equal (Just False)
                        ]
        , test "a decided match can be decided again" <|
            -- Only the view hides the buttons of decided matches. Both players
            -- end up out.
            \_ ->
                startWithPlacement 8
                    |> setWinner False ( 2, 0 )
                    |> setWinner True ( 2, 0 )
                    |> Expect.all
                        [ \g -> Dict.get ( 1, 0 ) g.slots |> Expect.equal (Just 1)
                        , \g -> List.map (\id -> aliveOf id g) [ 0, 1 ] |> Expect.equal [ Just False, Just False ]
                        ]
        , test "a player who skips the pre-round meets its winner" <|
            \_ ->
                start 3
                    |> setWinner True ( 1, 0 )
                    |> matchAt ( 0, 0 )
                    |> Maybe.map .player1
                    |> Expect.equal (Just ( 0, 0 ))
        ]


countMatches : Game id -> Int
countMatches =
    allMatches >> List.length


addPositionTests : Test
addPositionTests =
    describe "addPosition"
        [ forEachCount "adds one match for the new player" <|
            \n ->
                startWithPlacement n
                    |> addPlayer n
                    |> Maybe.map countMatches
                    |> Expect.equal (Just (countMatches (generateTournament n) + 1))
        , forEachCount "keeps the bracket consistent" <|
            \n ->
                case addPlayer n (startWithPlacement n) of
                    Just game ->
                        expectValidStructure game

                    Nothing ->
                        Expect.fail "no position added"
        , forEachCount "every player occupies exactly one initial slot" <|
            \n ->
                case addPlayer n (startWithPlacement n) of
                    Just game ->
                        initialSlots game
                            |> List.filterMap (\slot -> Dict.get slot game.slots)
                            |> List.sort
                            |> Expect.equal (List.range 0 n)

                    Nothing ->
                        Expect.fail "no position added"
        , test "adding players one by one from 2 to 9 keeps the bracket consistent" <|
            \_ ->
                List.range 2 8
                    |> List.foldl (\id -> Maybe.andThen (addPlayer id)) (Just (startWithPlacement 2))
                    |> Maybe.map
                        (Expect.all
                            [ expectValidStructure
                            , \g -> Dict.size g.slots |> Expect.equal 9
                            ]
                        )
                    |> Maybe.withDefault (Expect.fail "no position added")
        , test "growing from 3 to 4 players creates no third-place match" <|
            -- Bug: generateTournament 4 has a third-place match, this doesn't.
            \_ ->
                startWithPlacement 3
                    |> addPlayer 3
                    |> Maybe.map (\g -> ( matchAt ( 0, 1 ) g, matchesPerRound g ))
                    |> Expect.equal (Just ( Nothing, [ 1, 2 ] ))
        , test "decided matches keep their result" <|
            \_ ->
                startWithPlacement 4
                    |> setWinner False ( 1, 0 )
                    |> addPlayer 4
                    |> Maybe.map
                        (Expect.all
                            [ expectValidStructure
                            , \g -> List.map (\slot -> Dict.get slot g.slots) [ ( 0, 0 ), ( 0, 2 ) ] |> Expect.equal [ Just 0, Just 1 ]

                            -- the new match feeds the undecided semi-final
                            , \g -> matchAt ( 2, 2 ) g |> Maybe.andThen .winner |> Expect.equal (Just ( 1, 2 ))
                            , \g -> matchAt ( 1, 1 ) g |> Maybe.map .player1 |> Expect.equal (Just ( 1, 2 ))
                            , \g -> Dict.get ( 2, 3 ) g.slots |> Expect.equal (Just 4)
                            ]
                        )
                    |> Maybe.withDefault (Expect.fail "no position added")
        , test "a decided pre-round match is locked" <|
            \_ ->
                startWithPlacement 6
                    |> setWinner False ( 2, 0 )
                    |> addPlayer 6
                    |> Maybe.map
                        (Expect.all
                            [ expectValidStructure
                            , \g -> matchAt ( 2, 2 ) g |> Maybe.andThen .winner |> Expect.equal (Just ( 1, 2 ))
                            , \g -> Dict.get ( 1, 0 ) g.slots |> Expect.equal (Just 2)
                            ]
                        )
                    |> Maybe.withDefault (Expect.fail "no position added")
        , test "returns Nothing when every match of the first round is decided" <|
            \_ ->
                startWithPlacement 4
                    |> setWinner False ( 1, 0 )
                    |> setWinner False ( 1, 1 )
                    |> addPlayer 4
                    |> Expect.equal Nothing
        , test "without an unused player only the position is added" <|
            \_ ->
                start 4
                    |> Bracket.Knockout.addPosition
                    |> Maybe.map (\g -> ( Dict.size g.slots, countMatches g ))
                    |> Expect.equal (Just ( 4, 5 ))
        ]


hideFinishedMatchesTests : Test
hideFinishedMatchesTests =
    describe "hideFinishedMatches"
        [ test "keeps everything before the first result" <|
            \_ ->
                let
                    game =
                        startWithPlacement 8
                in
                hideFinishedMatches game |> Expect.equal game
        , test "hides decided matches" <|
            \_ ->
                startWithPlacement 8
                    |> setWinner False ( 2, 0 )
                    |> hideFinishedMatches
                    |> matchesPerRound
                    |> Expect.equal [ 2, 2, 3 ]
        , test "drops rounds without matches" <|
            \_ ->
                startWithPlacement 4
                    |> setWinner False ( 1, 0 )
                    |> setWinner False ( 1, 1 )
                    |> hideFinishedMatches
                    |> .rounds
                    |> Dict.keys
                    |> Expect.equal [ 0 ]
        , test "keeps the final without placement slots" <|
            \_ ->
                start 2
                    |> setWinner False ( 0, 0 )
                    |> hideFinishedMatches
                    |> matchesPerRound
                    |> Expect.equal [ 1 ]
        , test "hides the final once the placement slots are filled" <|
            \_ ->
                startWithPlacement 2
                    |> setWinner False ( 0, 0 )
                    |> hideFinishedMatches
                    |> .rounds
                    |> Expect.equal Dict.empty
        ]
