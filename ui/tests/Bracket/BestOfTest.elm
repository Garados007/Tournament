module Bracket.BestOfTest exposing (suite)

import Bracket.BestOf as BestOf
import Expect
import Json.Decode as JD
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Bracket.BestOf"
        [ test "more than half of the games are needed" <|
            \_ ->
                List.map BestOf.neededWins BestOf.lengths
                    |> Expect.equal [ 1, 2, 3, 4 ]
        , test "counts the wins of both players" <|
            \_ ->
                BestOf.wins [ False, True, True, False, True ]
                    |> Expect.equal ( 2, 3 )
        , test "a best of 3 has no winner at 1:1" <|
            \_ ->
                BestOf.winner 3 [ False, True ]
                    |> Expect.equal Nothing
        , test "a best of 3 is won with the second win" <|
            \_ ->
                BestOf.winner 3 [ False, True, True ]
                    |> Expect.equal (Just True)
        , test "a best of 1 is won with the first game" <|
            \_ ->
                BestOf.winner 1 [ False ]
                    |> Expect.equal (Just False)
        , test "a decided match takes no more games" <|
            \_ ->
                BestOf.addGame 3 True [ False, False ]
                    |> Expect.equal [ False, False ]
        , test "removes the last game" <|
            \_ ->
                BestOf.removeLastGame [ False, True ]
                    |> Expect.equal [ False ]
        , test "removing from no games keeps no games" <|
            \_ ->
                BestOf.removeLastGame []
                    |> Expect.equal []
        , test "games round-trip through JSON" <|
            \_ ->
                BestOf.encodeGames [ False, True, False ]
                    |> JD.decodeValue BestOf.decodeGames
                    |> Expect.equal (Ok [ False, True, False ])
        , test "an even match length is rejected" <|
            \_ ->
                JD.decodeString BestOf.decodeBestOf "2"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
