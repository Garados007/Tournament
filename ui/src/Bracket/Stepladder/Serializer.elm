module Bracket.Stepladder.Serializer exposing (decodeGame, decodeSettings, encodeGame, encodeSettings)

import Bracket.BestOf as BestOf
import Bracket.Stepladder exposing (Game, Match, Settings)
import Codec exposing (nullable)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE
import PlayerId


decodeSettings : Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> required "bestOf" BestOf.decodeBestOf
        |> required "finalBestOf" (JD.nullable BestOf.decodeBestOf)


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "bestOf", BestOf.encodeBestOf settings.bestOf )
        , ( "finalBestOf", nullable BestOf.encodeBestOf settings.finalBestOf )
        ]


{-| The winner of a walkover is the number of the player, 1 or 2.
-}
decodeWalkover : Decoder Bool
decodeWalkover =
    JD.int
        |> JD.andThen
            (\player ->
                case player of
                    1 ->
                        JD.succeed False

                    2 ->
                        JD.succeed True

                    _ ->
                        JD.fail <| "invalid walkover winner: " ++ String.fromInt player
            )


encodeWalkover : Bool -> Value
encodeWalkover isPlayer2 =
    JE.int <|
        if isPlayer2 then
            2

        else
            1


decodeMatch : Decoder Match
decodeMatch =
    JD.succeed Match
        |> required "games" BestOf.decodeGames
        |> required "walkover" (JD.nullable decodeWalkover)


encodeMatch : Match -> Value
encodeMatch match =
    JE.object
        [ ( "games", BestOf.encodeGames match.games )
        , ( "walkover", nullable encodeWalkover match.walkover )
        ]


{-| Data before version 9 has no `seeding`. Its ladder is used instead, so
players removed before they played can't come back.
-}
decodeGame : Decoder Game
decodeGame =
    JD.field "ladder" (JD.list PlayerId.decode)
        |> JD.andThen
            (\ladder ->
                JD.succeed (Game ladder)
                    |> optional "seeding" (JD.list PlayerId.decode) ladder
                    |> required "matches" (JD.list decodeMatch)
                    |> required "removed" (JD.list PlayerId.decode)
            )


encodeGame : Game -> Value
encodeGame game =
    JE.object
        [ ( "ladder", JE.list PlayerId.encode game.ladder )
        , ( "seeding", JE.list PlayerId.encode game.seeding )
        , ( "matches", JE.list encodeMatch game.matches )
        , ( "removed", JE.list PlayerId.encode game.removed )
        ]
