module FixtureTest exposing (suite)

import Bracket.BestOf as BestOf
import Bracket.DoubleElimination
import Bracket.Knockout exposing (Msg(..), Places(..))
import Bracket.Knockout.Placement
import Bracket.RoundRobin exposing (Matches(..), Ranking(..))
import Bracket.Scoring exposing (Cell(..), Scoring(..))
import Bracket.Stepladder
import Bracket.Swiss
import Data exposing (Data)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Generated.Fixtures exposing (Fixture, fixtures)
import Json.Decode as JD
import Json.Encode as JE
import Placement exposing (Placement)
import Test exposing (Test, describe, test)
import Tournament exposing (Entrants(..), Mode(..), ModeSettings(..), Msg(..), Order(..), Tournament(..))


key : Fixture -> String
key fixture =
    String.fromInt fixture.version ++ "/" ++ fixture.name


load : Fixture -> Result String Data
load fixture =
    JD.decodeString JD.value fixture.json
        |> Result.mapError JD.errorToString
        |> Result.andThen (Data.load >> Result.mapError Debug.toString)


modeOf : Tournament -> Mode
modeOf tournament =
    case tournament of
        Knockout _ ->
            KnockoutMode

        RoundRobin _ ->
            RoundRobinMode

        Swiss _ ->
            SwissMode

        Stepladder _ ->
            StepladderMode

        DoubleElimination _ ->
            DoubleEliminationMode


{-| The modes that a version of the format knows.
-}
modesOf : Int -> List Mode
modesOf version =
    [ KnockoutMode, RoundRobinMode, SwissMode, StepladderMode ]
        ++ (if version >= 6 then
                [ DoubleEliminationMode ]

            else
                []
           )


doubleEliminationState : Data -> Maybe Bracket.DoubleElimination.State
doubleEliminationState data =
    case data.tournament of
        Just (DoubleElimination state) ->
            Just state

        _ ->
            Nothing


swissState : Data -> Maybe Bracket.Swiss.State
swissState data =
    case data.tournament of
        Just (Swiss state) ->
            Just state

        _ ->
            Nothing


stepladderState : Data -> Maybe Bracket.Stepladder.State
stepladderState data =
    case data.tournament of
        Just (Stepladder state) ->
            Just state

        _ ->
            Nothing


knockoutState : Data -> Maybe Bracket.Knockout.State
knockoutState data =
    case data.tournament of
        Just (Knockout state) ->
            Just state

        _ ->
            Nothing


{-| The games won by both players of the match.
-}
wins : ( Int, Int ) -> Bracket.Knockout.State -> Maybe ( Int, Int )
wins match state =
    Bracket.Knockout.matchAt match state.game
        |> Maybe.map (.games >> BestOf.wins)


playGame : Bool -> ( Int, Int ) -> Tournament -> Tournament
playGame isPlayer2 match tournament =
    Tournament.update (KnockoutMsg <| WinGame isPlayer2 match) Tournament.initLocal tournament
        |> Tuple.second


{-| What every fixture must decode to, keyed by `<version>/<file>`. The
fixtures of the versions 6 to 8 repeat those of version 5. Only the knockout of
version 8 lets eliminated players take part in the next tournament.
-}
checks : Dict String (Data -> Expectation)
checks =
    Dict.fromList
        (sameChecks 5
            ++ sameChecks 6
            ++ sameChecks 7
            ++ sameChecks 8
            ++ [ ( "6/double-elimination.json", doubleEliminationCheck )
               , ( "7/double-elimination.json", doubleEliminationCheck )
               , ( "7/knockout-play-all.json", playAllCheck )
               , ( "8/double-elimination.json", doubleEliminationCheck )
               , ( "8/knockout-play-all.json", playAllCheck )
               ]
        )
        |> Dict.update "8/knockout.json" (Maybe.map withAllButRemoved)


withAllButRemoved : (Data -> Expectation) -> Data -> Expectation
withAllButRemoved check data =
    let
        settings =
            data.nextSettings
    in
    Expect.all
        [ \_ -> Expect.equal AllButRemoved settings.entrants
        , \_ -> check { data | nextSettings = { settings | entrants = StillIn } }
        ]
        ()


{-| 8 players, seeded. The quarter-finals are played, the losers play for the
places 5 to 8. Finn (5) was removed there, so Gina (6) and Hugo (7) won by
walkover.
-}
playAllCheck : Data -> Expectation
playAllCheck =
    Expect.all
        [ .nextSettings
            >> Expect.equal
                { order = Seeded
                , entrants = StillIn
                , mode = KnockoutSettings { places = PlayAll, bestOf = 1, finalBestOf = Just 3 }
                }
        , .tournament
            >> Maybe.map Tournament.placements
            >> Expect.equal (Just [ Placement 1 [], Placement 2 [], Placement 3 [] ])
        , knockoutState
            >> Maybe.map (.game >> Bracket.Knockout.Placement.allPlacements >> List.drop 4)
            >> Expect.equal (Just [ Placement 5 [], Placement 6 [], Placement 7 [ 7 ], Placement 8 [] ])
        , .tournament
            >> Maybe.map
                (\tournament ->
                    List.map (\id -> Tournament.isEliminated id tournament) [ 3, 4, 5, 7 ]
                )
            >> Expect.equal (Just [ False, False, True, False ])
        , \data ->
            data.tournament
                |> Maybe.map (playGame False ( -500, 0 ))
                |> Maybe.andThen (\tournament -> knockoutState { data | tournament = Just tournament })
                |> Maybe.map (.game >> Bracket.Knockout.Placement.allPlacements >> List.drop 4 >> List.take 2)
                |> Expect.equal (Just [ Placement 5 [ 3 ], Placement 6 [ 6 ] ])
                |> Expect.onFail "the match for the 5th place decides the 5th and 6th place"
        ]


sameChecks : Int -> List ( String, Data -> Expectation )
sameChecks version =
    List.map (Tuple.mapFirst (\name -> String.fromInt version ++ "/" ++ name)) fixtureChecks


doubleEliminationCheck : Data -> Expectation
doubleEliminationCheck =
    Expect.all
        [ .nextSettings
            >> Expect.equal
                { order = Seeded
                , entrants = StillIn
                , mode = DoubleEliminationSettings { reset = True, bestOf = 3, finalBestOf = Just 5 }
                }
        , doubleEliminationState
            >> Maybe.andThen (\state -> Bracket.Knockout.matchAt ( 3, 1 ) state.game)
            >> Maybe.map (.games >> BestOf.wins)
            >> Expect.equal (Just ( 1, 2 ))
        , .tournament
            >> Maybe.map
                (\tournament ->
                    List.map (\id -> Tournament.isEliminated id tournament) [ 0, 3, 4, 5 ]
                )
            >> Expect.equal (Just [ False, False, False, True ])
        , .tournament
            >> Maybe.map Tournament.acceptsPlayers
            >> Expect.equal (Just False)
        , .tournament
            >> Maybe.map Tournament.placements
            >> Expect.equal (Just [ Placement 1 [], Placement 2 [], Placement 3 [] ])
        , \data ->
            data.tournament
                |> Maybe.map
                    (Tournament.update (DoubleEliminationMsg <| WinGame False ( 2, 1 )) Tournament.initLocal
                        >> Tuple.second
                        >> Tournament.update (DoubleEliminationMsg <| WinGame False ( 2, 1 )) Tournament.initLocal
                        >> Tuple.second
                    )
                |> Maybe.andThen doubleEliminationTournamentSlot
                |> Expect.equal (Just ( Just 1, Just 2 ))
                |> Expect.onFail "the second won game decides the best of 3 and drops the loser to the loser bracket"
        ]


{-| The winner bracket final slot of player 2 and the loser bracket slot that
the loser of the second round match 2 drops to.
-}
doubleEliminationTournamentSlot : Tournament -> Maybe ( Maybe Int, Maybe Int )
doubleEliminationTournamentSlot tournament =
    case tournament of
        DoubleElimination state ->
            Just ( Dict.get ( 1, 1 ) state.game.slots, Dict.get ( -4, 1 ) state.game.slots )

        _ ->
            Nothing


fixtureChecks : List ( String, Data -> Expectation )
fixtureChecks =
    [ ( "knockout.json"
      , Expect.all
            [ .nextSettings
                >> Expect.equal
                    { order = Seeded
                    , entrants = StillIn
                    , mode = KnockoutSettings { places = Playoff, bestOf = 3, finalBestOf = Just 5 }
                    }
            , knockoutState
                >> Maybe.map
                    (\state ->
                        List.map (\key_ -> wins key_ state) [ ( 2, 0 ), ( 2, 1 ), ( 1, 0 ), ( 1, 1 ) ]
                    )
                >> Expect.equal (Just [ Just ( 2, 1 ), Just ( 0, 2 ), Just ( 0, 1 ), Just ( 1, 0 ) ])
            , .tournament
                >> Maybe.map
                    (\tournament ->
                        List.map (\id -> Tournament.isEliminated id tournament) [ 0, 1, 2, 5 ]
                    )
                >> Expect.equal (Just [ False, False, False, True ])
            , \data ->
                { data | tournament = Maybe.map (playGame False ( 1, 1 )) data.tournament }
                    |> knockoutState
                    |> Maybe.andThen (.game >> .slots >> Dict.get ( 0, 1 ))
                    |> Expect.equal (Just 0)
                    |> Expect.onFail "the second won game decides the best of 3"
            ]
      )
    , ( "round-robin.json"
      , Expect.all
            [ .title >> Expect.equal "League \"Spring\"; 2026"
            , .nextSettings
                >> Expect.equal
                    { order = Seeded
                    , entrants = StillIn
                    , mode = RoundRobinSettings { matches = Single, scoring = Points, ranking = ByResults }
                    }
            , .order >> Expect.equal [ 2, 0, 1, 3, 4 ]
            , \data ->
                case data.tournament of
                    Just (RoundRobin state) ->
                        Expect.all
                            [ .players >> List.map .id >> Expect.equal [ 2, 0, 1, 3, 4 ]
                            , .players >> List.filter .removed >> List.map .id >> Expect.equal [ 3 ]
                            , .results >> Dict.get ( 0, 2 ) >> Expect.equal (Just ( 2, 2 ))
                            , .results >> Dict.size >> Expect.equal 5
                            ]
                            state.game

                    _ ->
                        Expect.fail "expected a round robin"
            ]
      )
    , ( "swiss.json"
      , Expect.all
            [ .nextSettings
                >> Expect.equal
                    { order = Seeded
                    , entrants = StillIn
                    , mode = SwissSettings { rounds = Nothing, scoring = Points }
                    }
            , swissState
                >> Maybe.map (\state -> ( state.game.totalRounds, List.length state.game.rounds ))
                >> Expect.equal (Just ( 3, 2 ))
            , swissState
                >> Maybe.andThen (.game >> .rounds >> List.drop 1 >> List.head)
                >> Maybe.map (.bye >> Expect.equal (Just 2))
                >> Maybe.withDefault (Expect.fail "expected a second round")
            , \data ->
                swissState data
                    |> Maybe.map
                        (\state ->
                            Bracket.Swiss.matchCell state.game { home = 4, away = 3, result = Nothing }
                        )
                    |> Expect.equal (Just (Forfeit True))
                    |> Expect.onFail "the open match of the removed player is a forfeit"
            , .tournament
                >> Maybe.map
                    (\tournament ->
                        List.map (\id -> Tournament.isEliminated id tournament) [ 3, 7 ]
                    )
                >> Expect.equal (Just [ True, False ])
            , .tournament
                >> Maybe.map Tournament.placements
                >> Expect.equal (Just [])
            ]
      )
    , ( "no-tournament.json"
      , Expect.all
            [ .tournament >> Expect.equal Nothing
            , .nextSettings
                >> Expect.equal
                    { order = Shuffled
                    , entrants = StillIn
                    , mode = StepladderSettings { bestOf = 1, finalBestOf = Just 3 }
                    }
            ]
      )
    , ( "stepladder.json"
      , Expect.all
            [ .nextSettings
                >> Expect.equal
                    { order = Seeded
                    , entrants = StillIn
                    , mode = StepladderSettings { bestOf = 3, finalBestOf = Just 5 }
                    }
            , stepladderState
                >> Maybe.andThen (\state -> Bracket.Stepladder.currentIndex state.settings state.game)
                >> Expect.equal (Just 2)
            , .tournament
                >> Maybe.map
                    (\tournament ->
                        List.map (\id -> Tournament.isEliminated id tournament) [ 0, 1, 2, 3, 4, 5 ]
                    )
                >> Expect.equal (Just [ False, False, True, False, True, True ])
            , .tournament
                >> Maybe.map Tournament.placements
                >> Expect.equal (Just [ Placement 1 [], Placement 2 [], Placement 3 [] ])
            , \data ->
                data.tournament
                    |> Maybe.map
                        (Tournament.update (StepladderMsg <| Bracket.Stepladder.WinGame False 2) Tournament.initLocal
                            >> Tuple.second
                            >> Tournament.placements
                        )
                    |> Expect.equal (Just [ Placement 1 [], Placement 2 [], Placement 3 [ 1 ] ])
                    |> Expect.onFail "the second won game decides the best of 3 and gives the loser the 3rd place"
            ]
      )
    ]


normalize : String -> Result JD.Error String
normalize =
    JD.decodeString JD.value >> Result.map (JE.encode 0)


suite : Test
suite =
    describe "Fixtures"
        [ describe "every fixture decodes to the expected data" <|
            List.map
                (\fixture ->
                    test (key fixture) <|
                        \_ ->
                            case ( load fixture, Dict.get (key fixture) checks ) of
                                ( Ok data, Just check ) ->
                                    check data

                                ( Err error, _ ) ->
                                    Expect.fail error

                                ( Ok _, Nothing ) ->
                                    Expect.fail "the fixture has no entry in FixtureTest.checks"
                )
                fixtures
        , describe "fixtures of the current version encode to the same JSON" <|
            List.map
                (\fixture ->
                    test (key fixture) <|
                        \_ ->
                            load fixture
                                |> Result.map (Data.encodeData >> JE.encode 0)
                                |> Result.mapError (always ())
                                |> Expect.equal (normalize fixture.json |> Result.mapError (always ()))
                )
                (List.filter (.version >> (==) Data.currentVersion) fixtures)
        , describe "every version has a running tournament of every mode" <|
            List.map
                (\version ->
                    test (String.fromInt version) <|
                        \_ ->
                            let
                                modes =
                                    fixtures
                                        |> List.filter (.version >> (==) version)
                                        |> List.filterMap (load >> Result.toMaybe >> Maybe.andThen .tournament)
                                        |> List.map modeOf
                            in
                            List.filter (\mode -> not <| List.member mode modes) (modesOf version)
                                |> Expect.equal []
                )
                (List.filter Data.isSupported <| List.range 1 Data.currentVersion)
        , test "only supported versions have fixtures" <|
            \_ ->
                fixtures
                    |> List.map .version
                    |> List.filter (not << Data.isSupported)
                    |> Expect.equal []
        , test "every check has a fixture" <|
            \_ ->
                Dict.keys checks
                    |> List.filter (\name -> not <| List.member name <| List.map key fixtures)
                    |> Expect.equal []
        ]
