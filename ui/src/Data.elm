module Data exposing (..)

import Codec exposing (decodeDict, encodeDict, nullable)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE
import PlayerId exposing (PlayerId)
import Tournament exposing (Settings, Tournament)


type alias Data =
    { title : String
    , nextId : PlayerId
    , users : Dict PlayerId String
    , origin : String
    , nextSettings : Settings
    , tournament : Maybe Tournament
    }


init : Data
init =
    { title = "Tournament"
    , nextId = 0
    , users = Dict.empty
    , origin = ""
    , nextSettings = Tournament.defaultSettings
    , tournament = Nothing
    }


decodeData : Decoder Data
decodeData =
    JD.succeed Data
        |> required "title" JD.string
        |> required "nextId" PlayerId.decode
        |> required "users" (decodeDict PlayerId.decode JD.string)
        |> required "origin" JD.string
        |> required "nextSettings" Tournament.decodeSettings
        |> required "tournament" (JD.nullable Tournament.decodeTournament)


encodeData : Data -> Value
encodeData data =
    JE.object
        [ ( "title", JE.string data.title )
        , ( "nextId", PlayerId.encode data.nextId )
        , ( "users", encodeDict PlayerId.encode JE.string data.users )
        , ( "origin", JE.string data.origin )
        , ( "nextSettings", Tournament.encodeSettings data.nextSettings )
        , ( "tournament", nullable Tournament.encodeTournament data.tournament )
        ]
