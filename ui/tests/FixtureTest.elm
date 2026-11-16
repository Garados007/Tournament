module FixtureTest exposing (suite)

import Bracket.BestOf as BestOf
import Bracket.Knockout exposing (Msg(..), ThirdPlace(..))
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
import Tournament exposing (Mode(..), ModeSettings(..), Msg(..), Order(..), Tournament(..))


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


allModes : List Mode
allModes =
    [ KnockoutMode, RoundRobinMode, SwissMode, StepladderMode ]


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


{-| What every fixture must decode to, keyed by `<version>/<file>`.
-}
checks : Dict String (Data -> Expectation)
checks =
    Dict.fromList
        [ ( "5/knockout.json"
          , Expect.all
                [ .nextSettings
                    >> Expect.equal
                        { order = Seeded
                        , mode = KnockoutSettings { thirdPlace = Playoff, bestOf = 3, finalBestOf = Just 5 }
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
        , ( "5/round-robin.json"
          , Expect.all
                [ .title >> Expect.equal "League \"Spring\"; 2026"
                , .nextSettings
                    >> Expect.equal
                        { order = Seeded
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
        , ( "5/swiss.json"
          , Expect.all
                [ .nextSettings
                    >> Expect.equal
                        { order = Seeded
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
        , ( "5/no-tournament.json"
          , Expect.all
                [ .tournament >> Expect.equal Nothing
                , .nextSettings
                    >> Expect.equal
                        { order = Shuffled
                        , mode = StepladderSettings { bestOf = 1, finalBestOf = Just 3 }
                        }
                ]
          )
        , ( "5/stepladder.json"
          , Expect.all
                [ .nextSettings
                    >> Expect.equal
                        { order = Seeded
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
                            List.filter (\mode -> not <| List.member mode modes) allModes
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
