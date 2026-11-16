module Bracket.DoubleElimination.Serializer exposing (decodeGame, decodeSettings, encodeGame, encodeSettings)

{-| The game is a knockout game, so it uses the knockout format.
-}

import Bracket.BestOf as BestOf
import Bracket.DoubleElimination exposing (Settings)
import Bracket.Knockout exposing (Game)
import Bracket.Knockout.Serializer
import Codec exposing (nullable)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE


decodeSettings : Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> required "reset" JD.bool
        |> required "bestOf" BestOf.decodeBestOf
        |> required "finalBestOf" (JD.nullable BestOf.decodeBestOf)


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "reset", JE.bool settings.reset )
        , ( "bestOf", BestOf.encodeBestOf settings.bestOf )
        , ( "finalBestOf", nullable BestOf.encodeBestOf settings.finalBestOf )
        ]


decodeGame : Decoder Game
decodeGame =
    Bracket.Knockout.Serializer.decodeGame


encodeGame : Game -> Value
encodeGame =
    Bracket.Knockout.Serializer.encodeGame
