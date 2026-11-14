module Bracket.RoundRobin.Serializer exposing (decodeGame, decodeSettings, encodeGame, encodeSettings)

import Bracket.RoundRobin exposing (..)
import Codec exposing (decodeDict, encodeDict)
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


matchesValues : List ( String, Matches )
matchesValues =
    [ ( "single", Single ), ( "rematch", Rematch ) ]


scoringValues : List ( String, Scoring )
scoringValues =
    [ ( "points", Points ), ( "winnerOnly", WinnerOnly ) ]


rankingValues : List ( String, Ranking )
rankingValues =
    [ ( "byResults", ByResults ), ( "byTotalPoints", ByTotalPoints ) ]


encodeEnum : List ( String, a ) -> a -> Value
encodeEnum values value =
    List.filter (Tuple.second >> (==) value) values
        |> List.head
        |> Maybe.map (Tuple.first >> JE.string)
        |> Maybe.withDefault JE.null


decodeSettings : Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> required "matches" (decodeEnum "matches" matchesValues)
        |> required "scoring" (decodeEnum "scoring" scoringValues)
        |> required "ranking" (decodeEnum "ranking" rankingValues)


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "matches", encodeEnum matchesValues settings.matches )
        , ( "scoring", encodeEnum scoringValues settings.scoring )
        , ( "ranking", encodeEnum rankingValues settings.ranking )
        ]
