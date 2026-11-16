module Network exposing (..)

import Data exposing (Data)
import Json.Decode as JD
import Json.Encode as JE
import Ports exposing (..)
import Url
import WebSocket


{-| Messages of other types than the relayed data are ignored.
-}
decodeMessage : String -> Result Data.LoadError (Maybe Data)
decodeMessage message =
    let
        decoder =
            JD.field "$type" JD.string
                |> JD.andThen
                    (\type_ ->
                        if type_ == "Relay" then
                            JD.field "value" JD.value
                                |> JD.map Just

                        else
                            JD.succeed Nothing
                    )
    in
    case JD.decodeString decoder message of
        Ok (Just value) ->
            Data.load value
                |> Result.map Just

        Ok Nothing ->
            Ok Nothing

        Err error ->
            Err <| Data.Invalid <| JD.errorToString error


wsReceive : (Result Data.LoadError (Maybe Data) -> msg) -> Sub msg
wsReceive tagger =
    receiveSocketMsg <|
        WebSocket.receive <|
            \result ->
                case result of
                    Err err ->
                        tagger <| Err <| Data.Invalid <| JD.errorToString err

                    Ok (WebSocket.Error { error }) ->
                        tagger <| Err <| Data.Invalid error

                    Ok (WebSocket.Data { data }) ->
                        tagger <| decodeMessage data


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
