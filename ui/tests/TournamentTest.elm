module TournamentTest exposing (suite)

import Bracket.Knockout exposing (Msg(..), ThirdPlace(..))
import Bracket.Knockout.Placement
import Dict
import Expect
import Helper exposing (setWinner, startWithPlacement)
import Placement exposing (Placement)
import Test exposing (Test, describe, test)
import Tournament exposing (ModeSettings(..), Msg(..), Tournament(..))


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
                                        |> Bracket.Knockout.Placement.apply
                                }
                            )
            , test "stores the mode settings" <|
                \_ ->
                    Tournament.start
                        { knockout | mode = KnockoutSettings { thirdPlace = Shared } }
                        [ 0, 1 ]
                        |> (\(Knockout s) -> s.settings)
                        |> Expect.equal { thirdPlace = Shared }
            ]
        , test "addPlayer delegates to knockout" <|
            \_ ->
                Tournament.addPlayer 4 (Knockout state)
                    |> Expect.equal (Knockout { state | game = Bracket.Knockout.addPlayer 4 state.game })
        , test "removePlayer delegates to knockout" <|
            \_ ->
                Tournament.removePlayer 2 (Knockout state)
                    |> Expect.equal (Knockout { state | game = Bracket.Knockout.removePlayer 2 state.game })
        , test "update delegates to knockout" <|
            \_ ->
                case Helper.matchAt ( 0, 0 ) state.game of
                    Just match ->
                        Tournament.update (KnockoutMsg (SetWinner True match)) (Knockout state)
                            |> Expect.equal (Knockout { state | game = setWinner True ( 0, 0 ) state.game })

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
                    |> (\(Knockout s) -> Dict.values s.game.players)
                    |> Expect.equal [ { id = 0, alive = True }, { id = 1, alive = True } ]
        ]
