module Bracket.Knockout.Serializer exposing (..)

import Bracket.Knockout exposing (..)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE


nullable : (value -> Value) -> Maybe value -> Value
nullable mapper =
    Maybe.map mapper >> Maybe.withDefault JE.null


decodePlayerSlot : Decoder PlayerSlot
decodePlayerSlot =
    JD.map2 Tuple.pair
        (JD.index 0 JD.int)
        (JD.index 1 JD.int)


encodePlayerSlot : PlayerSlot -> Value
encodePlayerSlot ( x, y ) =
    JE.list JE.int [ x, y ]


decodeMatch : Decoder Match
decodeMatch =
    JD.succeed Match
        |> required "player1" decodePlayerSlot
        |> required "player2" decodePlayerSlot
        |> required "winner" (JD.nullable decodePlayerSlot)
        |> required "loser" (JD.nullable decodePlayerSlot)


encodeMatch : Match -> Value
encodeMatch match =
    JE.object
        [ ( "player1", encodePlayerSlot match.player1 )
        , ( "player2", encodePlayerSlot match.player2 )
        , Tuple.pair "winner" <|
            nullable encodePlayerSlot match.winner
        , Tuple.pair "loser" <|
            nullable encodePlayerSlot match.loser
        ]


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


decodeRound : Decoder Round
decodeRound =
    decodeDict JD.int decodeMatch


encodeRound : Round -> Value
encodeRound value =
    encodeDict JE.int encodeMatch value


decodePlayer : Decoder Player
decodePlayer =
    JD.succeed Player
        |> required "id" JD.int
        |> required "name" JD.string
        |> required "alive" JD.bool


encodePlayer : Player -> Value
encodePlayer player =
    JE.object
        [ ( "id", JE.int player.id )
        , ( "name", JE.string player.name )
        , ( "alive", JE.bool player.alive )
        ]


decodeGame : Decoder Game
decodeGame =
    JD.succeed Game
        |> required "rounds" (decodeDict JD.int decodeRound)
        |> required "slots" (decodeDict decodePlayerSlot JD.int)
        |> required "players" (decodeDict JD.int decodePlayer)


encodeGame : Game -> Value
encodeGame game =
    JE.object
        [ ( "rounds", encodeDict JE.int encodeRound game.rounds )
        , ( "slots", encodeDict encodePlayerSlot JE.int game.slots )
        , ( "players", encodeDict JE.int encodePlayer game.players )
        ]
