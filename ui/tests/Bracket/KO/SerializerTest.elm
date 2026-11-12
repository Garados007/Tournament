module Bracket.KO.SerializerTest exposing (suite)

import Bracket.KO exposing (generateTournament)
import Bracket.KO.Serializer exposing (decodeGame, encodeGame)
import Expect
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, test)


suite : Test
suite =
    test "encodeGame and decodeGame round-trip a generated tournament" <|
        \_ ->
            let
                game =
                    generateTournament 8
            in
            encodeGame JE.int game
                |> JD.decodeValue (decodeGame JD.int)
                |> Expect.equal (Ok game)
