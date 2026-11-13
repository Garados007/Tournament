module Bracket.Knockout.PlacementTest exposing (suite)

import Bracket.Knockout.Placement exposing (apply, placements)
import Expect
import Helper exposing (matchAt, setWinner, start, startWithPlacement)
import Placement exposing (Placement)
import Test exposing (Test, describe, test)


open : Int -> Placement
open place =
    Placement place []


suite : Test
suite =
    describe "Bracket.Knockout.Placement"
        [ describe "apply"
            [ test "the final sends its players to the first two places" <|
                \_ ->
                    apply (start 8)
                        |> matchAt ( 0, 0 )
                        |> Maybe.map (\p -> ( p.winner, p.loser ))
                        |> Expect.equal (Just ( Just ( -1, 0 ), Just ( -1, 1 ) ))
            , test "the third-place match sends its winner to the third place" <|
                \_ ->
                    apply (start 8)
                        |> matchAt ( 0, 1 )
                        |> Maybe.map (\p -> ( p.winner, p.loser ))
                        |> Expect.equal (Just ( Just ( -1, 2 ), Nothing ))
            , test "changes no other match" <|
                \_ ->
                    let
                        game =
                            start 8

                        placed =
                            apply game
                    in
                    [ ( 1, 0 ), ( 1, 1 ), ( 2, 0 ), ( 2, 3 ) ]
                        |> List.map (\pos -> matchAt pos placed == matchAt pos game)
                        |> Expect.equal [ True, True, True, True ]
            ]
        , describe "placements"
            [ describe "places" <|
                List.map
                    (\( n, expected ) ->
                        test (String.fromInt n ++ " players") <|
                            \_ ->
                                placements (startWithPlacement n)
                                    |> Expect.equal expected
                    )
                    [ ( 2, [ open 1, open 2 ] )
                    , ( 3, [ open 1, open 2 ] )
                    , ( 4, [ open 1, open 2, open 3 ] )
                    , ( 9, [ open 1, open 2, open 3 ] )
                    ]
            , test "are empty before the final" <|
                \_ ->
                    startWithPlacement 4
                        |> setWinner False ( 1, 0 )
                        |> setWinner False ( 1, 1 )
                        |> placements
                        |> Expect.equal [ open 1, open 2, open 3 ]
            , test "the third place is decided before the final" <|
                \_ ->
                    -- 4 players: P0 beats P1, P2 beats P3, P1 beats P3
                    startWithPlacement 4
                        |> setWinner False ( 1, 0 )
                        |> setWinner False ( 1, 1 )
                        |> setWinner False ( 0, 1 )
                        |> placements
                        |> Expect.equal [ open 1, open 2, Placement 3 [ 1 ] ]
            , test "the final decides the first two places" <|
                \_ ->
                    startWithPlacement 2
                        |> setWinner True ( 0, 0 )
                        |> placements
                        |> Expect.equal [ Placement 1 [ 1 ], Placement 2 [ 0 ] ]
            , test "are empty without the placement slots" <|
                \_ ->
                    start 2
                        |> setWinner True ( 0, 0 )
                        |> placements
                        |> Expect.equal []
            ]
        ]
