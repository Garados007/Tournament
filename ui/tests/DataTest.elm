module DataTest exposing (suite)

import Bracket.Knockout exposing (ThirdPlace(..))
import Bracket.RoundRobin exposing (Matches(..), Ranking(..), Scoring(..))
import Bracket.RoundRobin.View
import Data exposing (Data, decodeData, encodeData)
import Dict
import Expect
import Helper exposing (withVersion)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)
import Tournament exposing (ModeSettings(..), Msg(..), Order(..))


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
        , order = [ 2, 0, 3, 1 ]
        , origin = "abc"
        , tournament =
            Tournament.start Tournament.defaultSettings [ 2, 0, 1 ]
                |> Tournament.addPlayer 3
                |> Tournament.removePlayer 0
                |> Just
    }


withSettings : Order -> ThirdPlace -> Data -> Data
withSettings order thirdPlace data =
    { data
        | nextSettings =
            { order = order
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
                    (\( order, thirdPlace ) ->
                        test (Debug.toString ( order, thirdPlace )) <|
                            \_ -> roundTrip (withSettings order thirdPlace running)
                    )
                    [ ( Seeded, Playoff ), ( Shuffled, Shared ), ( Shuffled, NoThirdPlace ) ]
            , test "a tournament keeps its own settings" <|
                \_ ->
                    let
                        data =
                            withSettings Seeded Shared running
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
                        { order = Seeded
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
        , describe "the order"
            [ test "decoding drops unknown and repeated players and appends missing ones" <|
                \_ ->
                    { running | order = [ 3, 7, 1, 3 ] }
                        |> encodeData
                        |> JD.decodeValue decodeData
                        |> Result.map .order
                        |> Expect.equal (Ok [ 3, 1, 0, 2 ])
            , test "a new player is appended" <|
                \_ ->
                    Data.addPlayer "P4" running
                        |> Expect.all
                            [ .order >> Expect.equal [ 2, 0, 3, 1, 4 ]
                            , .users >> Dict.get 4 >> Expect.equal (Just "P4")
                            , .nextId >> Expect.equal 5
                            ]
            , test "filtering players keeps the order of the others" <|
                \_ ->
                    Data.filterPlayers ((/=) 0) running
                        |> Expect.all
                            [ .order >> Expect.equal [ 2, 3, 1 ]
                            , .users >> Dict.keys >> Expect.equal [ 1, 2, 3 ]
                            ]
            , describe "movePlayer" <|
                List.map
                    (\( ( id, step ), expected ) ->
                        test (Debug.toString ( id, step )) <|
                            \_ ->
                                Data.movePlayer (always True) id step running
                                    |> .order
                                    |> Expect.equal expected
                    )
                    [ ( ( 3, -1 ), [ 2, 3, 0, 1 ] )
                    , ( ( 3, 1 ), [ 2, 0, 1, 3 ] )
                    , ( ( 2, -1 ), [ 2, 0, 3, 1 ] )
                    , ( ( 1, 1 ), [ 2, 0, 3, 1 ] )
                    , ( ( 9, 1 ), [ 2, 0, 3, 1 ] )
                    ]
            , test "moving skips the players that can't move" <|
                \_ ->
                    Data.movePlayer ((/=) 0) 3 -1 running
                        |> .order
                        |> Expect.equal [ 3, 0, 2, 1 ]
            , test "a player that can't move stays" <|
                \_ ->
                    Data.movePlayer ((/=) 0) 0 1 running
                        |> .order
                        |> Expect.equal running.order
            ]
        , test "an unknown mode fails" <|
            \_ ->
                """{"version":1,"title":"T","nextId":0,"users":{},"origin":"","nextSettings":{"shuffle":true,"mode":{"mode":"unknown"}},"tournament":null}"""
                    |> JD.decodeString decodeData
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , describe "version"
            [ test "the current version is written" <|
                \_ ->
                    encodeData Data.init
                        |> JD.decodeValue (JD.field "version" JD.int)
                        |> Expect.equal (Ok Data.currentVersion)
            , test "data of a newer version can't be loaded" <|
                \_ ->
                    encodeData running
                        |> withVersion (Data.currentVersion + 1)
                        |> Data.load
                        |> Expect.equal (Err Data.NewerVersion)
            , test "data without a version is invalid" <|
                \_ ->
                    case
                        encodeData running
                            |> JD.decodeValue (JD.keyValuePairs JD.value)
                            |> Result.withDefault []
                            |> List.filter (Tuple.first >> (/=) "version")
                            |> JE.object
                            |> Data.load
                    of
                        Err (Data.Invalid _) ->
                            Expect.pass

                        other ->
                            Expect.fail <| Debug.toString other
            , test "version 0 is invalid" <|
                \_ ->
                    case encodeData running |> withVersion 0 |> Data.load of
                        Err (Data.Invalid _) ->
                            Expect.pass

                        other ->
                            Expect.fail <| Debug.toString other
            , test "broken data of the current version is invalid" <|
                \_ ->
                    case JE.object [ ( "version", JE.int Data.currentVersion ) ] |> Data.load of
                        Err (Data.Invalid _) ->
                            Expect.pass

                        other ->
                            Expect.fail <| Debug.toString other
            ]
        ]
