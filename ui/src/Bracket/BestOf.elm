module Bracket.BestOf exposing
    ( BestOf
    , Games
    , addGame
    , decodeBestOf
    , decodeGames
    , encodeBestOf
    , encodeGames
    , lengths
    , neededWins
    , removeLastGame
    , winner
    , wins
    )

{-| A match over several games. The first player to win more than half of the
games wins the match.
-}

import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE


{-| The maximum number of games of a match. It is odd, so there is no draw.
-}
type alias BestOf =
    Int


{-| The winner of every game in the order they were played: `True` for player 2.
-}
type alias Games =
    List Bool


lengths : List BestOf
lengths =
    [ 1, 3, 5, 7 ]


neededWins : BestOf -> Int
neededWins bestOf =
    bestOf // 2 + 1


{-| The games won by player 1 and player 2.
-}
wins : Games -> ( Int, Int )
wins games =
    List.foldl
        (\isPlayer2 ( wins1, wins2 ) ->
            if isPlayer2 then
                ( wins1, wins2 + 1 )

            else
                ( wins1 + 1, wins2 )
        )
        ( 0, 0 )
        games


{-| `True` if player 2 has won the match, `False` for player 1.
-}
winner : BestOf -> Games -> Maybe Bool
winner bestOf games =
    let
        ( wins1, wins2 ) =
            wins games
    in
    if wins1 >= neededWins bestOf then
        Just False

    else if wins2 >= neededWins bestOf then
        Just True

    else
        Nothing


{-| A decided match takes no more games.
-}
addGame : BestOf -> Bool -> Games -> Games
addGame bestOf isPlayer2 games =
    if winner bestOf games == Nothing then
        games ++ [ isPlayer2 ]

    else
        games


removeLastGame : Games -> Games
removeLastGame games =
    List.take (List.length games - 1) games


decodeBestOf : Decoder BestOf
decodeBestOf =
    JD.int
        |> JD.andThen
            (\bestOf ->
                if bestOf > 0 && modBy 2 bestOf == 1 then
                    JD.succeed bestOf

                else
                    JD.fail <| "invalid match length: " ++ String.fromInt bestOf
            )


encodeBestOf : BestOf -> Value
encodeBestOf =
    JE.int


{-| Every game is the number of its winner, 1 or 2.
-}
decodeGames : Decoder Games
decodeGames =
    JD.list <|
        JD.andThen
            (\player ->
                case player of
                    1 ->
                        JD.succeed False

                    2 ->
                        JD.succeed True

                    _ ->
                        JD.fail <| "invalid game winner: " ++ String.fromInt player
            )
            JD.int


encodeGames : Games -> Value
encodeGames =
    JE.list
        (\isPlayer2 ->
            JE.int <|
                if isPlayer2 then
                    2

                else
                    1
        )
