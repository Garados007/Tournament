module Codec exposing (decodeDict, decodeEnum, encodeDict, encodeEnum, nullable)

{-| JSON helpers shared by all modes.
-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE


nullable : (value -> Value) -> Maybe value -> Value
nullable mapper =
    Maybe.map mapper >> Maybe.withDefault JE.null


{-| Decodes an object whose keys are JSON encoded with the key decoder.
-}
decodeDict : Decoder comparableKey -> Decoder value -> Decoder (Dict comparableKey value)
decodeDict keyDecoder valueDecoder =
    JD.keyValuePairs valueDecoder
        |> JD.andThen
            (List.foldl
                (\( keyString, value ) decoder ->
                    case JD.decodeString keyDecoder keyString of
                        Err err ->
                            JD.fail <| JD.errorToString err

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


{-| Decodes a string that names one of the values.
-}
decodeEnum : String -> List ( String, a ) -> Decoder a
decodeEnum name values =
    JD.string
        |> JD.andThen
            (\value ->
                case List.filter (Tuple.first >> (==) value) values of
                    ( _, result ) :: _ ->
                        JD.succeed result

                    [] ->
                        JD.fail <| "unknown " ++ name ++ ": " ++ value
            )


encodeEnum : List ( String, a ) -> a -> Value
encodeEnum values value =
    List.filter (Tuple.second >> (==) value) values
        |> List.head
        |> Maybe.map (Tuple.first >> JE.string)
        |> Maybe.withDefault JE.null
