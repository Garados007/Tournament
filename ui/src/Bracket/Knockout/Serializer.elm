module Bracket.Knockout.Serializer exposing (..)

import Bracket.Knockout exposing (..)
import Codec exposing (decodeDict, encodeDict, nullable)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE
import PlayerId


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


decodeRound : Decoder Round
decodeRound =
    decodeDict JD.int decodeMatch


encodeRound : Round -> Value
encodeRound value =
    encodeDict JE.int encodeMatch value


decodePlayer : Decoder Player
decodePlayer =
    JD.succeed Player
        |> required "id" PlayerId.decode
        |> required "alive" JD.bool


encodePlayer : Player -> Value
encodePlayer player =
    JE.object
        [ ( "id", PlayerId.encode player.id )
        , ( "alive", JE.bool player.alive )
        ]


decodeGame : Decoder Game
decodeGame =
    JD.succeed Game
        |> required "rounds" (decodeDict JD.int decodeRound)
        |> required "slots" (decodeDict decodePlayerSlot PlayerId.decode)
        |> required "players" (decodeDict PlayerId.decode decodePlayer)


encodeGame : Game -> Value
encodeGame game =
    JE.object
        [ ( "rounds", encodeDict JE.int encodeRound game.rounds )
        , ( "slots", encodeDict encodePlayerSlot PlayerId.encode game.slots )
        , ( "players", encodeDict PlayerId.encode encodePlayer game.players )
        ]


decodeThirdPlace : Decoder ThirdPlace
decodeThirdPlace =
    JD.string
        |> JD.andThen
            (\value ->
                case value of
                    "playoff" ->
                        JD.succeed Playoff

                    "shared" ->
                        JD.succeed Shared

                    "none" ->
                        JD.succeed NoThirdPlace

                    _ ->
                        JD.fail <| "unknown third place: " ++ value
            )


encodeThirdPlace : ThirdPlace -> Value
encodeThirdPlace thirdPlace =
    JE.string <|
        case thirdPlace of
            Playoff ->
                "playoff"

            Shared ->
                "shared"

            NoThirdPlace ->
                "none"


decodeSettings : Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> required "thirdPlace" decodeThirdPlace


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "thirdPlace", encodeThirdPlace settings.thirdPlace ) ]
