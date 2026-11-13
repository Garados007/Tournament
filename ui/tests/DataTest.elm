module DataTest exposing (suite)

import Bracket.Knockout exposing (ThirdPlace(..))
import Bracket.RoundRobin exposing (Matches(..), Ranking(..), Scoring(..))
import Bracket.RoundRobin.View
import Data exposing (Data, decodeData, encodeData)
import Dict
import Expect
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)
import Tournament exposing (ModeSettings(..), Msg(..))


roundTrip : Data -> Expect.Expectation
roundTrip data =
    encodeData data
        |> JE.encode 0
        |> JD.decodeString decodeData
        |> Expect.equal (Ok data)


running : Data
running =
    let
        init =
            Data.init
    in
    { init
        | title = "Cup"
        , nextId = 4
        , users = Dict.fromList [ ( 0, "P0" ), ( 1, "P1" ), ( 2, "P2" ), ( 3, "P3" ) ]
        , origin = "abc"
        , tournament =
            Tournament.start Tournament.defaultSettings [ 2, 0, 1 ]
                |> Tournament.addPlayer 3
                |> Tournament.removePlayer 0
                |> Just
    }


withSettings : Bool -> ThirdPlace -> Data -> Data
withSettings shuffle thirdPlace data =
    { data
        | nextSettings =
            { shuffle = shuffle
            , mode = KnockoutSettings { thirdPlace = thirdPlace }
            }
    }


suite : Test
suite =
    describe "Data"
        [ describe "encodeData and decodeData round-trip"
            [ test "the initial data" <| \_ -> roundTrip Data.init
            , test "a running tournament" <| \_ -> roundTrip running
            , test "users without a tournament" <|
                \_ -> roundTrip { running | tournament = Nothing }
            , describe "the next settings" <|
                List.map
                    (\( shuffle, thirdPlace ) ->
                        test (Debug.toString ( shuffle, thirdPlace )) <|
                            \_ -> roundTrip (withSettings shuffle thirdPlace running)
                    )
                    [ ( False, Playoff ), ( True, Shared ), ( True, NoThirdPlace ) ]
            , test "a tournament keeps its own settings" <|
                \_ ->
                    let
                        data =
                            withSettings False Shared running
                    in
                    roundTrip
                        { data
                            | tournament =
                                Just <|
                                    Tournament.start data.nextSettings [ 0, 1 ]
                        }
            ]
        , test "a round robin with results" <|
            \_ ->
                let
                    settings =
                        { shuffle = False
                        , mode = RoundRobinSettings { matches = Rematch, scoring = WinnerOnly, ranking = ByTotalPoints }
                        }
                in
                roundTrip
                    { running
                        | nextSettings = settings
                        , tournament =
                            Tournament.start settings [ 0, 1, 2 ]
                                |> Tournament.update
                                    (RoundRobinMsg <| Bracket.RoundRobin.View.SetOutcome 1 0 ( 1, 0 ))
                                    Tournament.initLocal
                                |> Tuple.second
                                |> Tournament.removePlayer 2
                                |> Just
                    }
        , test "an unknown mode fails" <|
            \_ ->
                """{"title":"T","nextId":0,"users":{},"origin":"","nextSettings":{"shuffle":true,"mode":{"mode":"unknown"}},"tournament":null}"""
                    |> JD.decodeString decodeData
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
