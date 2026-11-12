module Network exposing (..)

import Data exposing (Data)
import Json.Decode as JD
import Json.Encode as JE
import Ports exposing (..)
import Url
import WebSocket


decodeMessage : JD.Decoder Data
decodeMessage =
    JD.field "$type" JD.string
        |> JD.andThen
            (\type_ ->
                if type_ == "Relay" then
                    JD.field "value" Data.decodeData

                else
                    JD.fail "unknown message type"
            )


wsReceive : (Result String Data -> msg) -> Sub msg
wsReceive tagger =
    receiveSocketMsg <|
        WebSocket.receive <|
            \result ->
                case result of
                    Err err ->
                        tagger <| Err <| JD.errorToString err

                    Ok (WebSocket.Error { error }) ->
                        tagger <| Err error

                    Ok (WebSocket.Data { data }) ->
                        tagger <|
                            Result.mapError JD.errorToString <|
                                JD.decodeString decodeMessage data


wsConnect : String -> String -> Cmd msg
wsConnect relay id =
    WebSocket.send sendSocketCommand <|
        WebSocket.Connect
            { name = "ws"
            , address = websocketUrl relay ++ "/ws?id=" ++ Url.percentEncode id
            , protocol = ""
            }


websocketUrl : String -> String
websocketUrl relay =
    if String.startsWith "https://" relay then
        "wss://" ++ String.dropLeft 8 relay

    else if String.startsWith "http://" relay then
        "ws://" ++ String.dropLeft 7 relay

    else
        relay


wsExit : Cmd msg
wsExit =
    WebSocket.send sendSocketCommand <|
        WebSocket.Close
            { name = "ws" }


wsClose : (Result JD.Error SocketClose -> msg) -> Sub msg
wsClose tagger =
    receiveSocketClose <|
        tagger
            << JD.decodeValue
                (JD.map2 SocketClose
                    (JD.field "code" JD.int)
                    (JD.field "reason" JD.string)
                )


wsSend : String -> Data -> Cmd msg
wsSend token data =
    WebSocket.send sendSocketCommand <|
        WebSocket.Send
            { name = "ws"
            , content =
                JE.encode 0 <|
                    JE.object
                        [ ( "$type", JE.string "Relay" )
                        , ( "token", JE.string token )
                        , ( "value", Data.encodeData data )
                        ]
            }


type alias SocketClose =
    { code : Int
    , reason : String
    }
