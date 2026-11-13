module Data exposing (..)

import Bracket.Knockout exposing (PlayerId)
import Bracket.Knockout.Placement
import Bracket.Knockout.Serializer exposing (decodeDict, decodeGame, decodePlayerId, encodeDict, encodeGame, encodePlayerId, nullable)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE


type alias Data =
    { title : String
    , nextId : PlayerId
    , users : Dict PlayerId String
    , origin : String
    , placement : Maybe Bracket.Knockout.Placement.Placements
    , game : Maybe Bracket.Knockout.Game
    }


init : Data
init =
    { title = "Tournament"
    , nextId = 0
    , users = Dict.empty
    , origin = ""
    , placement = Nothing
    , game = Nothing
    }


decodeData : Decoder Data
decodeData =
    JD.succeed Data
        |> required "title" JD.string
        |> required "nextId" decodePlayerId
        |> required "users" (decodeDict decodePlayerId JD.string)
        |> required "origin" JD.string
        |> optional "placement" (JD.nullable Bracket.Knockout.Placement.decode) Nothing
        |> required "game" (JD.nullable decodeGame)


encodeData : Data -> Value
encodeData data =
    JE.object
        [ ( "title", JE.string data.title )
        , ( "nextId", encodePlayerId data.nextId )
        , ( "users", encodeDict encodePlayerId JE.string data.users )
        , ( "origin", JE.string data.origin )
        , ( "placement", nullable Bracket.Knockout.Placement.encode data.placement )
        , ( "game", nullable encodeGame data.game )
        ]
