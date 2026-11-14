module TransferTest exposing (suite)

import Data exposing (Data)
import Dict
import Expect
import Helper exposing (withVersion)
import Json.Encode as JE
import Test exposing (Test, describe, test)
import Time
import Tournament
import Transfer exposing (Outcome(..))


{-| 2026-03-05 23:07 UTC
-}
time : Time.Posix
time =
    Time.millisToPosix 1772752020000


running : Data
running =
    let
        init =
            Data.init
    in
    { init
        | title = "Cup"
        , nextId = 2
        , users = Dict.fromList [ ( 0, "A" ), ( 1, "B" ) ]
        , tournament = Just <| Tournament.start Tournament.defaultSettings [ 0, 1 ]
    }


file : Data -> String
file =
    Data.encodeData >> JE.encode 4


suite : Test
suite =
    describe "Transfer"
        [ describe "fileName"
            [ test "title, date and time" <|
                \_ ->
                    Transfer.fileName "json" Time.utc time "Cup"
                        |> Expect.equal "Cup 2026-03-05 23-07.json"
            , test "the local time is used" <|
                \_ ->
                    Transfer.fileName "csv" (Time.customZone 60 []) time "Cup"
                        |> Expect.equal "Cup 2026-03-06 00-07.csv"
            , test "characters Windows doesn't allow become _" <|
                \_ ->
                    Transfer.fileName "json" Time.utc time "a<b>c:d\"e/f\\g|h?i*j\tk"
                        |> Expect.equal "a_b_c_d_e_f_g_h_i_j_k 2026-03-05 23-07.json"
            , test "other characters stay" <|
                \_ ->
                    Transfer.fileName "json" Time.utc time "Grüße & Co. (2026) 🏆"
                        |> Expect.equal "Grüße & Co. (2026) 🏆 2026-03-05 23-07.json"
            , test "an empty title becomes tournament" <|
                \_ ->
                    Transfer.fileName "json" Time.utc time ""
                        |> Expect.equal "tournament 2026-03-05 23-07.json"
            , test "a blank title becomes tournament" <|
                \_ ->
                    Transfer.fileName "json" Time.utc time "  "
                        |> Expect.equal "tournament 2026-03-05 23-07.json"
            ]
        , describe "readFile"
            [ test "without a running tournament the file is loaded" <|
                \_ ->
                    Transfer.readFile { running | tournament = Nothing } (file running)
                        |> Expect.equal (Load running)
            , test "a running tournament is only replaced after a confirmation" <|
                \_ ->
                    Transfer.readFile running (file Data.init)
                        |> Expect.equal (AskToReplace Data.init)
            , test "a file that isn't JSON is rejected" <|
                \_ ->
                    case Transfer.readFile Data.init "not json" of
                        Rejected (Data.Invalid _) ->
                            Expect.pass

                        other ->
                            Expect.fail <| Debug.toString other
            , test "JSON of another format is rejected" <|
                \_ ->
                    case Transfer.readFile Data.init "{\"title\":\"Cup\"}" of
                        Rejected (Data.Invalid _) ->
                            Expect.pass

                        other ->
                            Expect.fail <| Debug.toString other
            , test "a file of a newer version is rejected as such" <|
                \_ ->
                    Data.encodeData running
                        |> withVersion (Data.currentVersion + 1)
                        |> JE.encode 0
                        |> Transfer.readFile Data.init
                        |> Expect.equal (Rejected Data.NewerVersion)
            ]
        ]
