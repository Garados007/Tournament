module SyncTest exposing (suite)

import Data
import Expect
import Json.Decode as JD
import Json.Encode as JE
import Network
import Sync exposing (CloseAction(..), Problem(..), Role(..), Stored(..))
import Test exposing (Test, describe, test)


relayMessage : JE.Value -> String
relayMessage value =
    JE.encode 0 <|
        JE.object
            [ ( "$type", JE.string "Relay" )
            , ( "value", value )
            ]


suite : Test
suite =
    describe "Sync"
        [ describe "role"
            [ test "a client with a token is an editor" <|
                \_ ->
                    Sync.role { offline = False, creds = Just ( "g", Just "t" ) }
                        |> Expect.equal Editor
            , test "a client without a token is a viewer" <|
                \_ ->
                    Sync.role { offline = False, creds = Just ( "g", Nothing ) }
                        |> Expect.equal Viewer
            , test "a client waiting for a new group is an editor" <|
                \_ ->
                    Sync.role { offline = False, creds = Nothing }
                        |> Expect.equal Editor
            , test "an offline client is an editor" <|
                \_ ->
                    Sync.role { offline = True, creds = Just ( "g", Nothing ) }
                        |> Expect.equal Editor
            ]
        , describe "closeAction"
            [ test "an editor hosts a new group" <|
                \_ ->
                    Sync.closeAction Editor
                        |> Expect.equal HostNewGroup
            , test "a viewer is told about the lost connection" <|
                \_ ->
                    Sync.closeAction Viewer
                        |> Expect.equal ReportConnectionLost
            ]
        , describe "report"
            [ test "the first problem is shown" <|
                \_ ->
                    Sync.report ConnectionLost Nothing
                        |> Expect.equal (Just ConnectionLost)
            , test "a problem that needs a reset replaces one that needs a reload" <|
                \_ ->
                    Sync.report StoredDataInvalid (Just ReceivedDataInvalid)
                        |> Expect.equal (Just StoredDataInvalid)
            , test "a lost connection doesn't hide a needed reload" <|
                \_ ->
                    Sync.report ConnectionLost (Just ReceivedDataInvalid)
                        |> Expect.equal (Just ReceivedDataInvalid)
            ]
        , describe "decodeStored"
            [ test "null means nothing is saved" <|
                \_ ->
                    Sync.decodeStored JE.null
                        |> Expect.equal NothingStored
            , test "saved data is loaded" <|
                \_ ->
                    Sync.decodeStored (Data.encodeData Data.init)
                        |> Expect.equal (Stored Data.init)
            , test "garbage is unreadable" <|
                \_ ->
                    Sync.decodeStored (JE.string "garbage")
                        |> Expect.equal Unreadable
            , test "an object of another format is unreadable" <|
                \_ ->
                    Sync.decodeStored (JE.object [ ( "title", JE.int 1 ) ])
                        |> Expect.equal Unreadable
            ]
        , describe "Network.decodeMessage"
            [ test "relayed data is decoded" <|
                \_ ->
                    relayMessage (Data.encodeData Data.init)
                        |> JD.decodeString Network.decodeMessage
                        |> Expect.equal (Ok <| Just Data.init)
            , test "relayed data of another format fails" <|
                \_ ->
                    relayMessage (JE.object [])
                        |> JD.decodeString Network.decodeMessage
                        |> Expect.err
            , test "other message types are ignored" <|
                \_ ->
                    JE.encode 0 (JE.object [ ( "$type", JE.string "Info" ) ])
                        |> JD.decodeString Network.decodeMessage
                        |> Expect.equal (Ok Nothing)
            ]
        ]
