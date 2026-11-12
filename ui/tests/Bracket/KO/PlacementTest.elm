module Bracket.KO.PlacementTest exposing (suite)

import Bracket.KO.Placement exposing (Placement, getPlacement, hasPlacement)
import Expect
import Helper exposing (phaseAt, setWinner, start)
import Test exposing (Test, describe, test)


first : Placement
first =
    Placement "1" "st" ( -1, 0 )


second : Placement
second =
    Placement "2" "nd" ( -1, 1 )


third : Placement
third =
    Placement "3" "rd" ( -1, 2 )


suite : Test
suite =
    describe "Bracket.KO.Placement"
        [ describe "getPlacement"
            [ describe "places" <|
                List.map
                    (\( n, expected ) ->
                        test (String.fromInt n ++ " players") <|
                            \_ ->
                                getPlacement (start n)
                                    |> Tuple.second
                                    |> Expect.equal expected
                    )
                    [ ( 2, [ first, second ] )
                    , ( 3, [ first, second ] )
                    , ( 4, [ first, second, third ] )
                    , ( 9, [ first, second, third ] )
                    ]
            , test "the final sends its players to the first two places" <|
                \_ ->
                    getPlacement (start 8)
                        |> Tuple.first
                        |> phaseAt ( 0, 0 )
                        |> Maybe.map (\p -> ( p.winner, p.looser ))
                        |> Expect.equal (Just ( Just ( -1, 0 ), Just ( -1, 1 ) ))
            , test "the third-place match sends its winner to the third place" <|
                \_ ->
                    getPlacement (start 8)
                        |> Tuple.first
                        |> phaseAt ( 0, 1 )
                        |> Maybe.map (\p -> ( p.winner, p.looser ))
                        |> Expect.equal (Just ( Just ( -1, 2 ), Nothing ))
            , test "changes no other match" <|
                \_ ->
                    let
                        game =
                            start 8

                        ( placed, _ ) =
                            getPlacement game
                    in
                    [ ( 1, 0 ), ( 1, 1 ), ( 2, 0 ), ( 2, 3 ) ]
                        |> List.map (\pos -> phaseAt pos placed == phaseAt pos game)
                        |> Expect.equal [ True, True, True, True ]
            ]
        , describe "hasPlacement"
            [ test "is false before the final" <|
                \_ ->
                    let
                        ( game, placements ) =
                            getPlacement (start 4)
                    in
                    game
                        |> setWinner False ( 1, 0 )
                        |> setWinner False ( 1, 1 )
                        |> hasPlacement placements
                        |> Expect.equal False
            , test "is true once the third place is decided" <|
                \_ ->
                    let
                        ( game, placements ) =
                            getPlacement (start 4)
                    in
                    game
                        |> setWinner False ( 1, 0 )
                        |> setWinner False ( 1, 1 )
                        |> setWinner False ( 0, 1 )
                        |> hasPlacement placements
                        |> Expect.equal True
            , test "is true once the final is decided" <|
                \_ ->
                    let
                        ( game, placements ) =
                            getPlacement (start 2)
                    in
                    game
                        |> setWinner True ( 0, 0 )
                        |> hasPlacement placements
                        |> Expect.equal True
            , test "is false without placements" <|
                \_ ->
                    start 2
                        |> setWinner True ( 0, 0 )
                        |> hasPlacement []
                        |> Expect.equal False
            ]
        ]
