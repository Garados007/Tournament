module Bracket.RoundRobin.Serializer exposing (decodeGame, decodeSettings, encodeGame, encodeSettings)

import Bracket.RoundRobin exposing (..)
import Bracket.Scoring
import Codec exposing (decodeDict, decodeEnum, encodeDict, encodeEnum)
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


decodeGame : Decoder Game
decodeGame =
    JD.succeed Game
        |> required "players" (JD.list decodePlayer)
        |> required "results" (decodeDict decodePair decodePair)


encodeGame : Game -> Value
encodeGame game =
    JE.object
        [ ( "players", JE.list encodePlayer game.players )
        , ( "results", encodeDict encodePair encodePair game.results )
        ]


matchesValues : List ( String, Matches )
matchesValues =
    [ ( "single", Single ), ( "rematch", Rematch ) ]


rankingValues : List ( String, Ranking )
rankingValues =
    [ ( "byResults", ByResults ), ( "byTotalPoints", ByTotalPoints ) ]


decodeSettings : Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> required "matches" (decodeEnum "matches" matchesValues)
        |> required "scoring" Bracket.Scoring.decode
        |> required "ranking" (decodeEnum "ranking" rankingValues)


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "matches", encodeEnum matchesValues settings.matches )
        , ( "scoring", Bracket.Scoring.encode settings.scoring )
        , ( "ranking", encodeEnum rankingValues settings.ranking )
        ]
