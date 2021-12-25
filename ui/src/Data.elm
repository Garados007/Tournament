module Data exposing (..)

import Bracket.KO
import Bracket.KO.Placement
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as JE
import Bracket.KO.Serializer exposing (nullable, decodeDict, encodeDict, decodeGame, encodeGame)

type alias Data =
    { title: String
    , nextId: Int
    , user: Dict Int String
    , origin: String
    , placement: Maybe Bracket.KO.Placement.Placements
    , game: Maybe (Bracket.KO.Game Int)
    }

init : Data
init =
    { title = "Tournament"
    , nextId = 0
    , user = Dict.empty
    , origin = ""
    , placement = Nothing
    , game = Nothing
    }

decodeData : Decoder Data
decodeData =
    JD.succeed Data
    |> required "title" JD.string
    |> required "nextId" JD.int
    |> required "user" (decodeDict JD.int JD.string)
    |> required "origin" JD.string
    |> optional "placement" (JD.nullable Bracket.KO.Placement.decode) Nothing
    |> required "game" (JD.nullable <| decodeGame JD.int)

encodeData : Data -> Value
encodeData data =
    JE.object
        [ ("title", JE.string data.title)
        , ("nextId", JE.int data.nextId)
        , ("user", encodeDict JE.int JE.string data.user)
        , ("origin", JE.string data.origin)
        , ("placement", nullable Bracket.KO.Placement.encode data.placement)
        , ("game", nullable (encodeGame JE.int) data.game)
        ]
