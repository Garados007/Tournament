module FixtureTest exposing (suite)

import Bracket.RoundRobin exposing (Matches(..), Ranking(..), Scoring(..))
import Data exposing (Data)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Generated.Fixtures exposing (Fixture, fixtures)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)
import Tournament exposing (Mode(..), ModeSettings(..), Tournament(..))


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


allModes : List Mode
allModes =
    [ KnockoutMode, RoundRobinMode ]


{-| What every fixture must decode to, keyed by `<version>/<file>`.
-}
checks : Dict String (Data -> Expectation)
checks =
    Dict.fromList
        [ ( "1/knockout.json"
          , Expect.all
                [ .title >> Expect.equal "Knockout Cup"
                , .users >> Dict.size >> Expect.equal 7
                , .tournament
                    >> Maybe.map
                        (\tournament ->
                            List.map (\id -> Tournament.isEliminated id tournament) [ 0, 2, 3, 5 ]
                        )
                    >> Expect.equal (Just [ False, False, True, True ])
                ]
          )
        , ( "1/round-robin.json"
          , Expect.all
                [ .title >> Expect.equal "League \"Spring\"; 2026"
                , .nextSettings
                    >> Expect.equal
                        { shuffle = False
                        , mode = RoundRobinSettings { matches = Single, scoring = Points, ranking = ByResults }
                        }
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
        , ( "1/no-tournament.json"
          , Expect.all
                [ .title >> Expect.equal ""
                , .users >> Dict.keys >> Expect.equal [ 0, 2 ]
                , .tournament >> Expect.equal Nothing
                , .nextSettings
                    >> Expect.equal
                        { shuffle = True
                        , mode = RoundRobinSettings { matches = Rematch, scoring = WinnerOnly, ranking = ByTotalPoints }
                        }
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
                (List.range 1 Data.currentVersion)
        , test "every check has a fixture" <|
            \_ ->
                Dict.keys checks
                    |> List.filter (\name -> not <| List.member name <| List.map key fixtures)
                    |> Expect.equal []
        ]
