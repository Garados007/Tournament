module PlayerId exposing (PlayerId, decode, encode)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type alias PlayerId =
    Int


decode : Decoder PlayerId
decode =
    JD.int


encode : PlayerId -> Value
encode =
    JE.int
