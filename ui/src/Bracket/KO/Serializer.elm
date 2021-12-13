module Bracket.KO.Serializer exposing (..)

import Bracket.KO exposing (..)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE
import Dict exposing (Dict)

nullable : (value -> Value) -> Maybe value -> Value
nullable mapper =
    Maybe.map mapper >> Maybe.withDefault JE.null

decodePlayerSlot : Decoder PlayerSlot
decodePlayerSlot =
    JD.map2 Tuple.pair
        (JD.index 0 JD.int)
        (JD.index 1 JD.int)

encodePlayerSlot : PlayerSlot -> Value
encodePlayerSlot (x, y) =
    JE.list JE.int [ x, y]

decodePhase : Decoder Phase
decodePhase =
    JD.succeed Phase
    |> required "player1" decodePlayerSlot
    |> required "player2" decodePlayerSlot
    |> required "winner" (JD.nullable decodePlayerSlot)
    |> required "looser" (JD.nullable decodePlayerSlot)

encodePhase : Phase -> Value
encodePhase phase =
    JE.object
        [ ("player1", encodePlayerSlot phase.player1)
        , ("player2", encodePlayerSlot phase.player2)
        , Tuple.pair "winner"
            <| nullable encodePlayerSlot phase.winner
        , Tuple.pair "looser"
            <| nullable encodePlayerSlot phase.looser
        ]

decodeDict : Decoder comparableKey -> Decoder value -> Decoder (Dict comparableKey value)
decodeDict keyDecoder valueDecoder =
    JD.keyValuePairs valueDecoder
    |> JD.andThen
        (List.foldl
            (\(keyString, value) decoder ->
                case JD.decodeString keyDecoder keyString of
                    Err err -> JD.fail <| JD.errorToString err
                    Ok key ->
                        JD.map (Dict.insert key value) decoder
            )
            (JD.succeed Dict.empty)
        )

encodeDict : (key -> Value) -> (value -> Value) -> Dict key value -> Value
encodeDict keyEncoder valueEncoder dict =
    JE.dict
        (keyEncoder >> JE.encode 0)
        valueEncoder
        dict

decodePhaseLevel : Decoder PhaseLevel
decodePhaseLevel = decodeDict JD.int decodePhase

encodePhaseLevel : PhaseLevel -> Value
encodePhaseLevel value = encodeDict JE.int encodePhase value

decodePlayer : Decoder id -> Decoder (Player id)
decodePlayer decodeId =
    JD.succeed Player
    |> required "id" decodeId
    |> required "name" JD.string
    |> required "alive" JD.bool

encodePlayer : (id -> Value) -> Player id -> Value
encodePlayer encodeId player =
    JE.object
        [ ("id", encodeId player.id)
        , ("name", JE.string player.name)
        , ("alive", JE.bool player.alive)
        ]

decodeGame : Decoder comparableId -> Decoder (Game comparableId)
decodeGame decodeId =
    JD.succeed Game
    |> required "levels" (decodeDict JD.int decodePhaseLevel)
    |> required "slots" (decodeDict decodePlayerSlot decodeId)
    |> required "player" (decodeDict decodeId <| decodePlayer decodeId)

encodeGame : (id -> Value) -> Game id -> Value
encodeGame encodeId game =
    JE.object
        [ ("levels", encodeDict JE.int encodePhaseLevel game.levels)
        , ("slots", encodeDict encodePlayerSlot encodeId game.slots)
        , ("player", encodeDict encodeId (encodePlayer encodeId) game.player)
        ]
