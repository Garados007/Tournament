module DataTest exposing (suite)

import Bracket.Knockout.Placement exposing (getPlacement)
import Data exposing (Data, decodeData, encodeData)
import Dict
import Expect
import Helper exposing (setWinner, start)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)


roundTrip : Data -> Expect.Expectation
roundTrip data =
    encodeData data
        |> JE.encode 0
        |> JD.decodeString decodeData
        |> Expect.equal (Ok data)


running : Data
running =
    let
        ( game, placement ) =
            getPlacement (start 3)
    in
    { title = "Cup"
    , nextId = 3
    , user = Dict.fromList [ ( 0, "P0" ), ( 1, "P1" ), ( 2, "P2" ) ]
    , origin = "abc"
    , placement = Just placement
    , game = Just (setWinner False ( 1, 0 ) game)
    }


suite : Test
suite =
    describe "Data"
        [ describe "encodeData and decodeData round-trip"
            [ test "the initial data" <| \_ -> roundTrip Data.init
            , test "a running tournament" <| \_ -> roundTrip running
            , test "users without a tournament" <|
                \_ -> roundTrip { running | game = Nothing, placement = Nothing }
            ]
        , test "placement is optional" <|
            \_ ->
                """{"title":"T","nextId":0,"user":{},"origin":"","game":null}"""
                    |> JD.decodeString decodeData
                    |> Expect.equal (Ok { init | title = "T" })
        ]


init : Data
init =
    Data.init
