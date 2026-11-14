module TournamentTest exposing (suite)

import Bracket.Knockout exposing (Msg(..), ThirdPlace(..))
import Bracket.Knockout.Placement
import Bracket.RoundRobin exposing (Matches(..), Ranking(..), Scoring(..))
import Bracket.RoundRobin.View
import Dict
import Expect
import Helper exposing (setWinner, startWithPlacement)
import Placement exposing (Placement)
import Test exposing (Test, describe, test)
import Tournament exposing (ModeSettings(..), Msg(..), Tournament(..))


knockoutState : Tournament -> Maybe Bracket.Knockout.State
knockoutState tournament =
    case tournament of
        Knockout s ->
            Just s

        RoundRobin _ ->
            Nothing


roundRobinSettings : Bracket.RoundRobin.Settings
roundRobinSettings =
    { matches = Single, scoring = Points, ranking = ByResults }


roundRobin : Tournament.Settings
roundRobin =
    { shuffle = False
    , mode = RoundRobinSettings roundRobinSettings
    }


{-| Player 0 wins against 1 and 2, player 1 wins against 2.
-}
roundRobinPlayed : Tournament
roundRobinPlayed =
    List.foldl
        (\( row, column ) ->
            Tournament.update
                (RoundRobinMsg <| Bracket.RoundRobin.View.SetOutcome row column ( 1, 0 ))
                Tournament.initLocal
                >> Tuple.second
        )
        (Tournament.start roundRobin [ 0, 1, 2 ])
        [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ]


knockout : Tournament.Settings
knockout =
    { shuffle = True
    , mode = KnockoutSettings { thirdPlace = Playoff }
    }


{-| A knockout tournament with 4 players where P0 and P2 won their
semi-finals.
-}
state : Bracket.Knockout.State
state =
    { settings = { thirdPlace = Playoff }
    , game =
        startWithPlacement 4
            |> setWinner False ( 1, 0 )
            |> setWinner False ( 1, 1 )
    }


suite : Test
suite =
    describe "Tournament"
        [ test "the default settings start a shuffled knockout tournament" <|
            \_ ->
                Tournament.defaultSettings
                    |> Expect.equal knockout
        , test "a knockout tournament needs 2 players" <|
            \_ ->
                Tournament.minPlayers knockout.mode
                    |> Expect.equal 2
        , describe "start"
            [ test "starts a knockout bracket with placements" <|
                \_ ->
                    Tournament.start knockout [ 0, 1, 2, 3 ]
                        |> Expect.equal
                            (Knockout
                                { settings = { thirdPlace = Playoff }
                                , game = startWithPlacement 4
                                }
                            )
            , test "keeps the players in list order" <|
                \_ ->
                    Tournament.start knockout [ 3, 1, 2 ]
                        |> Expect.equal
                            (Knockout
                                { settings = { thirdPlace = Playoff }
                                , game =
                                    Bracket.Knockout.start [ 3, 1, 2 ]
                                        |> Bracket.Knockout.Placement.apply { thirdPlace = Playoff }
                                }
                            )
            , test "stores the mode settings" <|
                \_ ->
                    Tournament.start
                        { knockout | mode = KnockoutSettings { thirdPlace = Shared } }
                        [ 0, 1 ]
                        |> knockoutState
                        |> Maybe.map .settings
                        |> Expect.equal (Just { thirdPlace = Shared })
            ]
        , test "addPlayer delegates to knockout" <|
            \_ ->
                Tournament.addPlayer 4 (Knockout state)
                    |> Expect.equal
                        (Knockout
                            { state
                                | game =
                                    Bracket.Knockout.addPlayer 4 state.game
                                        |> Bracket.Knockout.Placement.apply state.settings
                            }
                        )
        , test "removePlayer delegates to knockout" <|
            \_ ->
                Tournament.removePlayer 2 (Knockout state)
                    |> Expect.equal (Knockout { state | game = Bracket.Knockout.removePlayer 2 state.game })
        , test "update delegates to knockout" <|
            \_ ->
                case Helper.matchAt ( 0, 0 ) state.game of
                    Just match ->
                        Tournament.update (KnockoutMsg (SetWinner True match)) Tournament.initLocal (Knockout state)
                            |> Expect.equal ( Tournament.initLocal, Knockout { state | game = setWinner True ( 0, 0 ) state.game } )

                    Nothing ->
                        Expect.fail "no final"
        , test "isEliminated delegates to knockout" <|
            \_ ->
                List.map (\id -> Tournament.isEliminated id (Knockout state)) [ 0, 1, 2, 3, 4 ]
                    |> Expect.equal (List.map (\id -> Bracket.Knockout.isEliminated id state.game) [ 0, 1, 2, 3, 4 ])
        , test "placements delegate to knockout" <|
            \_ ->
                Knockout { state | game = setWinner True ( 0, 0 ) state.game }
                    |> Tournament.placements
                    |> Expect.equal [ Placement 1 [ 2 ], Placement 2 [ 0 ], Placement 3 [] ]
        , test "player names aren't stored in the tournament" <|
            \_ ->
                Tournament.start knockout [ 0, 1 ]
                    |> knockoutState
                    |> Maybe.map (.game >> .players >> Dict.values)
                    |> Expect.equal (Just [ { id = 0, alive = True, removed = False }, { id = 1, alive = True, removed = False } ])
        , describe "round robin"
            [ test "starts with the players in list order" <|
                \_ ->
                    Tournament.start roundRobin [ 2, 0, 1 ]
                        |> Expect.equal (RoundRobin <| Bracket.RoundRobin.start roundRobinSettings [ 2, 0, 1 ])
            , test "needs 2 players" <|
                \_ ->
                    Tournament.minPlayers roundRobin.mode
                        |> Expect.equal 2
            , test "update stores the result" <|
                \_ ->
                    case roundRobinPlayed of
                        RoundRobin rr ->
                            Bracket.RoundRobin.cell 1 0 rr
                                |> Expect.equal (Bracket.RoundRobin.Played ( 0, 1 ))

                        Knockout _ ->
                            Expect.fail "not a round robin"
            , test "a message of another mode is ignored" <|
                \_ ->
                    case Helper.matchAt ( 0, 0 ) state.game of
                        Just match ->
                            Tournament.update (KnockoutMsg (SetWinner True match)) Tournament.initLocal roundRobinPlayed
                                |> Expect.equal ( Tournament.initLocal, roundRobinPlayed )

                        Nothing ->
                            Expect.fail "no final"
            , test "the placements once every match is played" <|
                \_ ->
                    Tournament.placements roundRobinPlayed
                        |> Expect.equal [ Placement 1 [ 0 ], Placement 2 [ 1 ], Placement 3 [ 2 ] ]
            , test "only removed players are eliminated" <|
                \_ ->
                    Tournament.removePlayer 1 roundRobinPlayed
                        |> (\t -> List.map (\id -> Tournament.isEliminated id t) [ 0, 1, 2 ])
                        |> Expect.equal [ False, True, False ]
            , test "an added player opens new matches" <|
                \_ ->
                    Tournament.addPlayer 3 roundRobinPlayed
                        |> Tournament.placements
                        |> Expect.equal []
            , test "uses the header control instead of hiding matches" <|
                \_ ->
                    Tournament.controls roundRobinPlayed
                        |> Expect.equal { hideFinished = False, headers = True }
            ]
        ]
