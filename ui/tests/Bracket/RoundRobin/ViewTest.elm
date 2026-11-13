module Bracket.RoundRobin.ViewTest exposing (suite)

import Bracket.RoundRobin as RR exposing (Cell(..), Matches(..), Ranking(..), Scoring(..), State)
import Bracket.RoundRobin.View exposing (Model, Msg(..), Side(..), init, update)
import Dict
import Expect
import Test exposing (Test, describe, test)


fresh : State
fresh =
    RR.start { matches = Single, scoring = Points, ranking = ByResults } [ 0, 1 ]


type_ : List ( Side, String ) -> ( Model, State ) -> ( Model, State )
type_ inputs start =
    List.foldl
        (\( side, value ) ( model, state ) -> update (SetScoreText 0 1 side value) model state)
        start
        inputs


suite : Test
suite =
    describe "Bracket.RoundRobin.View.update"
        [ test "half a result stays a draft" <|
            \_ ->
                type_ [ ( RowSide, "3" ) ] ( init, fresh )
                    |> Tuple.mapBoth (.drafts >> Dict.toList) (RR.cell 0 1)
                    |> Expect.equal ( [ ( ( 0, 1 ), ( "3", "" ) ) ], Open )
        , test "a complete result is stored and the draft is gone" <|
            \_ ->
                type_ [ ( RowSide, "3" ), ( ColumnSide, "-2" ) ] ( init, fresh )
                    |> Tuple.mapBoth (.drafts >> Dict.toList) (RR.cell 0 1)
                    |> Expect.equal ( [], Played ( 3, -2 ) )
        , test "a minus sign alone keeps the stored result" <|
            \_ ->
                type_ [ ( RowSide, "3" ), ( ColumnSide, "2" ), ( ColumnSide, "-" ) ] ( init, fresh )
                    |> Tuple.mapBoth (.drafts >> Dict.toList) (RR.cell 0 1)
                    |> Expect.equal ( [ ( ( 0, 1 ), ( "3", "-" ) ) ], Played ( 3, 2 ) )
        , test "emptying an input only changes the draft" <|
            \_ ->
                type_ [ ( RowSide, "3" ), ( ColumnSide, "2" ), ( RowSide, "" ) ] ( init, fresh )
                    |> Tuple.mapBoth (.drafts >> Dict.toList) (RR.cell 0 1)
                    |> Expect.equal ( [ ( ( 0, 1 ), ( "", "2" ) ) ], Played ( 3, 2 ) )
        , test "changing one side of a stored result stores the new result" <|
            \_ ->
                type_ [ ( RowSide, "3" ), ( ColumnSide, "2" ), ( RowSide, "13" ) ] ( init, fresh )
                    |> Tuple.second
                    |> RR.cell 0 1
                    |> Expect.equal (Played ( 13, 2 ))
        , test "clearing removes the draft and the result" <|
            \_ ->
                type_ [ ( RowSide, "3" ), ( ColumnSide, "2" ), ( RowSide, "-" ) ] ( init, fresh )
                    |> (\( model, state ) -> update (Clear 0 1) model state)
                    |> Tuple.mapBoth (.drafts >> Dict.toList) (RR.cell 0 1)
                    |> Expect.equal ( [], Open )
        , test "an outcome is stored right away" <|
            \_ ->
                update (SetOutcome 1 0 ( 0, 0 )) init fresh
                    |> Tuple.second
                    |> RR.cell 0 1
                    |> Expect.equal (Played ( 0, 0 ))
        , test "a result of a single match drops the draft of the mirrored cell" <|
            \_ ->
                update (SetScoreText 1 0 RowSide "-") init fresh
                    |> (\( model, state ) -> update (SetScoreText 0 1 RowSide "1") model state)
                    |> (\( model, state ) -> update (SetScoreText 0 1 ColumnSide "2") model state)
                    |> Tuple.first
                    |> .drafts
                    |> Expect.equal Dict.empty
        , test "a rematch keeps the draft of the other match" <|
            \_ ->
                let
                    rematch =
                        RR.start { matches = Rematch, scoring = Points, ranking = ByResults } [ 0, 1 ]
                in
                update (SetScoreText 1 0 RowSide "-") init rematch
                    |> (\( model, state ) -> update (Clear 0 1) model state)
                    |> Tuple.first
                    |> .drafts
                    |> Dict.toList
                    |> Expect.equal [ ( ( 1, 0 ), ( "-", "" ) ) ]
        ]
