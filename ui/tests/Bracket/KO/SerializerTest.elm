module Bracket.KO.SerializerTest exposing (suite)

import Bracket.KO exposing (Game, generateTournament)
import Bracket.KO.Serializer exposing (decodeGame, encodeGame)
import Expect
import Helper exposing (addPlayer, setWinner, startWithPlacement)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)


roundTrip : Game Int -> Expect.Expectation
roundTrip game =
    encodeGame JE.int game
        |> JE.encode 0
        |> JD.decodeString (decodeGame JD.int)
        |> Expect.equal (Ok game)


suite : Test
suite =
    describe "encodeGame and decodeGame round-trip"
        [ describe "a generated tournament" <|
            List.map
                (\n -> test (String.fromInt n ++ " players") <| \_ -> roundTrip (generateTournament n))
                (List.range 2 17)
        , test "a tournament with placement slots and results" <|
            \_ ->
                startWithPlacement 4
                    |> setWinner False ( 1, 0 )
                    |> setWinner True ( 1, 1 )
                    |> setWinner True ( 0, 0 )
                    |> setWinner False ( 0, 1 )
                    |> roundTrip
        , test "a tournament with an added player" <|
            \_ ->
                startWithPlacement 5
                    |> setWinner False ( 2, 0 )
                    |> addPlayer 5
                    |> Maybe.map roundTrip
                    |> Maybe.withDefault (Expect.fail "no position added")
        ]
