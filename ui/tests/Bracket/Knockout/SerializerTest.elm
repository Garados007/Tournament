module Bracket.Knockout.SerializerTest exposing (suite)

import Bracket.Knockout exposing (Game, Places(..), Settings, generateTournament)
import Bracket.Knockout.Serializer exposing (decodeGame, decodeSettings, encodeGame, encodeSettings)
import Expect
import Helper exposing (addPlayer, setWinner, setWinnerWith, startWithPlacement)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)


roundTrip : Game -> Expect.Expectation
roundTrip game =
    encodeGame game
        |> JE.encode 0
        |> JD.decodeString decodeGame
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
        , test "a tournament with games of longer matches" <|
            \_ ->
                startWithPlacement 4
                    |> setWinnerWith settings False ( 1, 0 )
                    |> setWinnerWith settings True ( 1, 0 )
                    |> setWinnerWith settings True ( 1, 0 )
                    |> setWinnerWith settings False ( 1, 1 )
                    |> roundTrip
        , test "settings round-trip" <|
            \_ ->
                encodeSettings settings
                    |> JD.decodeValue decodeSettings
                    |> Expect.equal (Ok settings)
        ]


settings : Settings
settings =
    { places = Shared, bestOf = 3, finalBestOf = Just 5 }
