module Bracket.Knockout.PlacementTest exposing (suite)

import Bracket.Knockout exposing (Game, ThirdPlace(..), isEliminated)
import Bracket.Knockout.Placement exposing (apply, placements)
import Dict
import Expect
import Helper exposing (expectValidStructure, grow, matchAt, matchesPerRound, setWinner, start, startWith)
import Placement exposing (Placement)
import Test exposing (Test, describe, test)


open : Int -> Placement
open place =
    Placement place []


variants : List ( String, ThirdPlace )
variants =
    [ ( "Playoff", Playoff ), ( "Shared", Shared ), ( "NoThirdPlace", NoThirdPlace ) ]


forEachVariant : String -> (ThirdPlace -> Int -> Expect.Expectation) -> Test
forEachVariant name check =
    describe name <|
        List.map
            (\( variantName, thirdPlace ) ->
                describe variantName <|
                    List.map
                        (\n -> test (String.fromInt n ++ " players") <| \_ -> check thirdPlace n)
                        [ 2, 3, 4, 5, 8 ]
            )
            variants


targets : ( Int, Int ) -> Game -> Maybe ( Maybe ( Int, Int ), Maybe ( Int, Int ) )
targets pos game =
    matchAt pos game
        |> Maybe.map (\match -> ( match.winner, match.loser ))


{-| 4 players: P0 beats P1 and P2 beats P3 in the semi-finals.
-}
semiFinalsPlayed : ThirdPlace -> Game
semiFinalsPlayed thirdPlace =
    startWith thirdPlace 4
        |> setWinner False ( 1, 0 )
        |> setWinner False ( 1, 1 )


suite : Test
suite =
    describe "Bracket.Knockout.Placement"
        [ describe "apply"
            [ forEachVariant "the final sends its players to the first two places" <|
                \thirdPlace n ->
                    startWith thirdPlace n
                        |> targets ( 0, 0 )
                        |> Expect.equal (Just ( Just ( -1, 0 ), Just ( -1, 1 ) ))
            , forEachVariant "keeps the bracket consistent" <|
                \thirdPlace n -> expectValidStructure (startWith thirdPlace n)
            , forEachVariant "only a playoff with 4 or more players has a third-place match" <|
                \thirdPlace n ->
                    startWith thirdPlace n
                        |> targets ( 0, 1 )
                        |> Expect.equal
                            (if thirdPlace == Playoff && n >= 4 then
                                Just ( Just ( -1, 2 ), Nothing )

                             else
                                Nothing
                            )
            , forEachVariant "where the semi-final losers go" <|
                \thirdPlace n ->
                    [ ( 1, 0 ), ( 1, 1 ) ]
                        |> List.map (\pos -> matchAt pos (startWith thirdPlace n) |> Maybe.andThen .loser)
                        |> Expect.equal
                            (case ( n >= 4, thirdPlace ) of
                                ( True, Playoff ) ->
                                    [ Just ( 0, 2 ), Just ( 0, 3 ) ]

                                ( True, Shared ) ->
                                    [ Just ( -1, 2 ), Just ( -1, 3 ) ]

                                _ ->
                                    [ Nothing, Nothing ]
                            )
            , test "3 players: the pre-round has no loser slot" <|
                \_ ->
                    startWith Playoff 3
                        |> targets ( 1, 0 )
                        |> Expect.equal (Just ( Just ( 0, 0 ), Nothing ))
            , test "changes no other match" <|
                \_ ->
                    let
                        game =
                            start 8

                        placed =
                            apply { thirdPlace = Playoff } game
                    in
                    [ ( 2, 0 ), ( 2, 3 ) ]
                        |> List.map (\pos -> matchAt pos placed == matchAt pos game)
                        |> Expect.equal [ True, True ]
            , forEachVariant "can be applied again" <|
                \thirdPlace n ->
                    startWith thirdPlace n
                        |> apply { thirdPlace = thirdPlace }
                        |> Expect.equal (startWith thirdPlace n)
            , describe "growing from 3 to 4 players"
                [ test "creates the third-place match" <|
                    \_ ->
                        startWith Playoff 3
                            |> grow Playoff 3
                            |> Expect.all
                                [ \g -> targets ( 0, 1 ) g |> Expect.equal (Just ( Just ( -1, 2 ), Nothing ))
                                , \g -> matchesPerRound g |> Expect.equal [ 2, 2 ]
                                , \g -> placements g |> Expect.equal [ open 1, open 2, open 3 ]
                                , expectValidStructure
                                ]
                , test "moves the loser of a decided semi-final to the third-place match" <|
                    -- P1 beats P2 in the pre-round, then P3 joins
                    \_ ->
                        startWith Playoff 3
                            |> setWinner False ( 1, 0 )
                            |> grow Playoff 3
                            |> Expect.all
                                [ \g -> Dict.get ( 0, 2 ) g.slots |> Expect.equal (Just 2)
                                , \g -> isEliminated 2 g |> Expect.equal False
                                ]
                , test "shares the third place with the loser of a decided semi-final" <|
                    \_ ->
                        startWith Shared 3
                            |> setWinner False ( 1, 0 )
                            |> grow Shared 3
                            |> placements
                            |> Expect.equal [ open 1, open 2, Placement 3 [ 2 ] ]
                , test "without a third place the loser stays out" <|
                    \_ ->
                        startWith NoThirdPlace 3
                            |> setWinner False ( 1, 0 )
                            |> grow NoThirdPlace 3
                            |> isEliminated 2
                            |> Expect.equal True
                ]
            ]
        , describe "placements"
            [ forEachVariant "places" <|
                \thirdPlace n ->
                    placements (startWith thirdPlace n)
                        |> Expect.equal
                            (if n >= 4 && thirdPlace /= NoThirdPlace then
                                [ open 1, open 2, open 3 ]

                             else
                                [ open 1, open 2 ]
                            )
            , test "are empty before the final" <|
                \_ ->
                    semiFinalsPlayed Playoff
                        |> placements
                        |> Expect.equal [ open 1, open 2, open 3 ]
            , test "the third place is decided before the final" <|
                \_ ->
                    -- P1 beats P3
                    semiFinalsPlayed Playoff
                        |> setWinner False ( 0, 1 )
                        |> placements
                        |> Expect.equal [ open 1, open 2, Placement 3 [ 1 ] ]
            , test "both semi-final losers share the third place" <|
                \_ ->
                    semiFinalsPlayed Shared
                        |> Expect.all
                            [ \g -> placements g |> Expect.equal [ open 1, open 2, Placement 3 [ 1, 3 ] ]
                            , \g -> List.map (\id -> isEliminated id g) [ 1, 3 ] |> Expect.equal [ False, False ]
                            ]
            , test "the third place is shared as soon as one semi-final is decided" <|
                \_ ->
                    startWith Shared 4
                        |> setWinner True ( 1, 1 )
                        |> placements
                        |> Expect.equal [ open 1, open 2, Placement 3 [ 2 ] ]
            , test "without a third place the semi-final losers are out" <|
                \_ ->
                    semiFinalsPlayed NoThirdPlace
                        |> Expect.all
                            [ \g -> placements g |> Expect.equal [ open 1, open 2 ]
                            , \g -> List.map (\id -> isEliminated id g) [ 1, 3 ] |> Expect.equal [ True, True ]
                            ]
            , describe "the final decides the first two places" <|
                List.map
                    (\( name, thirdPlace ) ->
                        test name <|
                            \_ ->
                                semiFinalsPlayed thirdPlace
                                    |> setWinner True ( 0, 0 )
                                    |> placements
                                    |> List.take 2
                                    |> Expect.equal [ Placement 1 [ 2 ], Placement 2 [ 0 ] ]
                    )
                    variants
            , test "the final of 2 players decides the first two places" <|
                \_ ->
                    startWith Shared 2
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
