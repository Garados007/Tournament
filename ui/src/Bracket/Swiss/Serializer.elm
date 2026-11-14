module Bracket.Swiss.Serializer exposing (decodeGame, decodeSettings, encodeGame, encodeSettings)

import Bracket.Scoring
import Bracket.Swiss exposing (..)
import Codec exposing (nullable)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE
import PlayerId


decodePair : Decoder ( Int, Int )
decodePair =
    JD.map2 Tuple.pair
        (JD.index 0 JD.int)
        (JD.index 1 JD.int)


encodePair : ( Int, Int ) -> Value
encodePair ( a, b ) =
    JE.list JE.int [ a, b ]


decodePlayer : Decoder Player
decodePlayer =
    JD.succeed Player
        |> required "id" PlayerId.decode
        |> required "removed" JD.bool


encodePlayer : Player -> Value
encodePlayer player =
    JE.object
        [ ( "id", PlayerId.encode player.id )
        , ( "removed", JE.bool player.removed )
        ]


decodeMatch : Decoder Match
decodeMatch =
    JD.succeed Match
        |> required "home" PlayerId.decode
        |> required "away" PlayerId.decode
        |> required "result" (JD.nullable decodePair)


encodeMatch : Match -> Value
encodeMatch match =
    JE.object
        [ ( "home", PlayerId.encode match.home )
        , ( "away", PlayerId.encode match.away )
        , ( "result", nullable encodePair match.result )
        ]


decodeRound : Decoder Round
decodeRound =
    JD.succeed Round
        |> required "matches" (JD.list decodeMatch)
        |> required "bye" (JD.nullable PlayerId.decode)


encodeRound : Round -> Value
encodeRound round =
    JE.object
        [ ( "matches", JE.list encodeMatch round.matches )
        , ( "bye", nullable PlayerId.encode round.bye )
        ]


decodeGame : Decoder Game
decodeGame =
    JD.succeed Game
        |> required "players" (JD.list decodePlayer)
        |> required "totalRounds" JD.int
        |> required "rounds" (JD.list decodeRound)


encodeGame : Game -> Value
encodeGame game =
    JE.object
        [ ( "players", JE.list encodePlayer game.players )
        , ( "totalRounds", JE.int game.totalRounds )
        , ( "rounds", JE.list encodeRound game.rounds )
        ]


decodeSettings : Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> required "rounds" (JD.nullable JD.int)
        |> required "scoring" Bracket.Scoring.decode


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "rounds", nullable JE.int settings.rounds )
        , ( "scoring", Bracket.Scoring.encode settings.scoring )
        ]
