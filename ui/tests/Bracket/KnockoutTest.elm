module Bracket.KnockoutTest exposing (suite)

import Bracket.Knockout exposing (Game, generateTournament, hideFinishedMatches, initialSlots, isEliminated, removePlayer, renamePlayer)
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
        , startTests
        , addPlayerTests
        , renamePlayerTests
        , removePlayerTests
        , isEliminatedTests
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


aliveOf : Int -> Game -> Maybe Bool
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


countMatches : Game -> Int
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


startTests : Test
startTests =
    describe "start"
        [ forEachCount "generates the bracket and sets the players" <|
            \n ->
                Bracket.Knockout.start (players n)
                    |> Expect.equal
                        (generateTournament n
                            |> Bracket.Knockout.setInitialPlayers (players n)
                        )
        ]


addPlayerTests : Test
addPlayerTests =
    describe "addPlayer"
        [ forEachCount "registers the player and adds a position" <|
            \n ->
                Expect.equal
                    (addPlayer n (startWithPlacement n))
                    (Just <| Bracket.Knockout.addPlayer n ("P" ++ String.fromInt n) (startWithPlacement n))
        , test "without a free position the player is only registered" <|
            \_ ->
                let
                    game =
                        startWithPlacement 4
                            |> setWinner False ( 1, 0 )
                            |> setWinner False ( 1, 1 )
                in
                Bracket.Knockout.addPlayer 4 "P4" game
                    |> Expect.all
                        [ \g -> Dict.get 4 g.players |> Expect.equal (Just { id = 4, name = "P4", alive = True })
                        , \g -> g.slots |> Expect.equal game.slots
                        , \g -> g.rounds |> Expect.equal game.rounds
                        ]
        ]


renamePlayerTests : Test
renamePlayerTests =
    describe "renamePlayer"
        [ test "changes the name of the player" <|
            \_ ->
                start 3
                    |> renamePlayer 1 "Anna"
                    |> .players
                    |> Dict.get 1
                    |> Expect.equal (Just { id = 1, name = "Anna", alive = True })
        , test "ignores unknown players" <|
            \_ ->
                start 3
                    |> renamePlayer 5 "Anna"
                    |> Expect.equal (start 3)
        ]


slotsOf : List ( Int, Int ) -> Game -> List (Maybe Int)
slotsOf slots game =
    List.map (\slot -> Dict.get slot game.slots) slots


removePlayerTests : Test
removePlayerTests =
    describe "removePlayer"
        [ test "the waiting opponent moves on" <|
            \_ ->
                startWithPlacement 4
                    |> removePlayer 0
                    |> Expect.all
                        [ \g -> slotsOf [ ( 0, 0 ), ( 0, 2 ) ] g |> Expect.equal [ Just 1, Nothing ]
                        , \g -> List.map (\id -> aliveOf id g) [ 0, 1 ] |> Expect.equal [ Just False, Just True ]
                        ]
        , test "the opponent moves on from either side of the match" <|
            \_ ->
                startWithPlacement 4
                    |> removePlayer 1
                    |> slotsOf [ ( 0, 0 ) ]
                    |> Expect.equal [ Just 0 ]
        , test "a player whose opponent hasn't arrived yet only drops out" <|
            -- 3 players: P0 waits in the final for the winner of P1 and P2
            \_ ->
                let
                    game =
                        startWithPlacement 3
                in
                removePlayer 0 game
                    |> Expect.all
                        [ \g -> g.slots |> Expect.equal game.slots
                        , \g -> aliveOf 0 g |> Expect.equal (Just False)
                        ]
        , test "the pre-round winner meets the waiting player" <|
            \_ ->
                startWithPlacement 3
                    |> removePlayer 1
                    |> slotsOf [ ( 0, 0 ), ( 1, 2 ) ]
                    |> Expect.equal [ Just 2, Just 0 ]
        , test "in a running tournament only the open match is decided" <|
            -- 8 players: P0 and P3 won their quarter-finals and meet in the semi-final
            \_ ->
                let
                    game =
                        startWithPlacement 8
                            |> setWinner False ( 2, 0 )
                            |> setWinner True ( 2, 1 )
                in
                removePlayer 3 game
                    |> Expect.all
                        [ \g -> slotsOf [ ( 0, 0 ), ( 0, 2 ) ] g |> Expect.equal [ Just 0, Nothing ]
                        , \g -> Dict.remove ( 0, 0 ) g.slots |> Expect.equal game.slots
                        , \g -> List.map (\id -> aliveOf id g) [ 0, 1, 2, 3 ] |> Expect.equal [ Just True, Just False, Just False, Just False ]
                        , expectValidStructure
                        ]
        , test "an undecided quarter-final is decided while the others stay open" <|
            \_ ->
                startWithPlacement 8
                    |> setWinner False ( 2, 0 )
                    |> removePlayer 5
                    |> slotsOf [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ]
                    |> Expect.equal [ Just 0, Nothing, Just 4, Nothing ]
        , test "a player who already lost changes no slot" <|
            \_ ->
                let
                    game =
                        startWithPlacement 8 |> setWinner False ( 2, 0 )
                in
                removePlayer 1 game
                    |> .slots
                    |> Expect.equal game.slots
        , test "a finalist hands the first place to the opponent" <|
            \_ ->
                startWithPlacement 2
                    |> removePlayer 1
                    |> slotsOf [ ( -1, 0 ), ( -1, 1 ) ]
                    |> Expect.equal [ Just 0, Nothing ]
        , test "without placement slots the final stays open" <|
            \_ ->
                start 2
                    |> removePlayer 1
                    |> .slots
                    |> Expect.equal (start 2).slots
        , test "ignores unknown players" <|
            \_ ->
                startWithPlacement 4
                    |> removePlayer 9
                    |> Expect.equal (startWithPlacement 4)
        ]


isEliminatedTests : Test
isEliminatedTests =
    describe "isEliminated"
        [ test "a player in the game is not out" <|
            \_ -> isEliminated 0 (start 2) |> Expect.equal False
        , test "a removed player is out" <|
            \_ -> isEliminated 0 (removePlayer 0 (start 2)) |> Expect.equal True
        , test "an unknown player is not out" <|
            \_ -> isEliminated 9 (start 2) |> Expect.equal False
        ]
