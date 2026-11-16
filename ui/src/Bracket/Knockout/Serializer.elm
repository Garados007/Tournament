module Bracket.Knockout.Serializer exposing (..)

import Bracket.BestOf as BestOf
import Bracket.Knockout exposing (..)
import Codec exposing (decodeDict, encodeDict, nullable)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (custom, required)
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
        |> required "games" BestOf.decodeGames


encodeMatch : Match -> Value
encodeMatch match =
    JE.object
        [ ( "player1", encodePlayerSlot match.player1 )
        , ( "player2", encodePlayerSlot match.player2 )
        , Tuple.pair "winner" <|
            nullable encodePlayerSlot match.winner
        , Tuple.pair "loser" <|
            nullable encodePlayerSlot match.loser
        , ( "games", BestOf.encodeGames match.games )
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
        |> required "removed" JD.bool


encodePlayer : Player -> Value
encodePlayer player =
    JE.object
        [ ( "id", PlayerId.encode player.id )
        , ( "alive", JE.bool player.alive )
        , ( "removed", JE.bool player.removed )
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


{-| Before version 7 the setting was called `thirdPlace` and had no "all".
-}
decodePlaces : Decoder Places
decodePlaces =
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

                    "all" ->
                        JD.succeed PlayAll

                    _ ->
                        JD.fail <| "unknown places: " ++ value
            )


encodePlaces : Places -> Value
encodePlaces places =
    JE.string <|
        case places of
            Playoff ->
                "playoff"

            Shared ->
                "shared"

            NoThirdPlace ->
                "none"

            PlayAll ->
                "all"


decodeSettings : Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> custom (JD.oneOf [ JD.field "places" decodePlaces, JD.field "thirdPlace" decodePlaces ])
        |> required "bestOf" BestOf.decodeBestOf
        |> required "finalBestOf" (JD.nullable BestOf.decodeBestOf)


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "places", encodePlaces settings.places )
        , ( "bestOf", BestOf.encodeBestOf settings.bestOf )
        , ( "finalBestOf", nullable BestOf.encodeBestOf settings.finalBestOf )
        ]
