module Bracket.Swiss.ViewTest exposing (suite)

import Bracket.Scoring exposing (Scoring(..))
import Bracket.Swiss as Swiss exposing (State)
import Bracket.Swiss.View exposing (Model, Msg(..), Side(..), init, update)
import Dict
import Expect
import Set
import Test exposing (Test, describe, test)


{-| 0 plays against 2 and 1 against 3.
-}
fresh : State
fresh =
    Swiss.start { rounds = Nothing, scoring = Points } [ 0, 1, 2, 3 ]


type_ : List ( Side, String ) -> ( Model, State ) -> ( Model, State )
type_ inputs start =
    List.foldl
        (\( side, value ) ( model, state ) -> update (SetScoreText 0 2 side value) model state)
        start
        inputs


firstResult : State -> Maybe ( Int, Int )
firstResult state =
    Swiss.currentRound state.game
        |> Maybe.andThen (.matches >> List.head)
        |> Maybe.andThen .result


suite : Test
suite =
    describe "Bracket.Swiss.View.update"
        [ test "half a result stays a draft" <|
            \_ ->
                type_ [ ( HomeSide, "3" ) ] ( init, fresh )
                    |> Tuple.mapBoth (.drafts >> Dict.toList) firstResult
                    |> Expect.equal ( [ ( ( 0, 2 ), ( "3", "" ) ) ], Nothing )
        , test "a complete result is stored and drops the draft" <|
            \_ ->
                type_ [ ( HomeSide, "3" ), ( AwaySide, "1" ) ] ( init, fresh )
                    |> Tuple.mapBoth (.drafts >> Dict.toList) firstResult
                    |> Expect.equal ( [], Just ( 3, 1 ) )
        , test "editing a stored result keeps the other score" <|
            \_ ->
                type_ [ ( HomeSide, "3" ), ( AwaySide, "1" ), ( HomeSide, "0" ) ] ( init, fresh )
                    |> Tuple.second
                    |> firstResult
                    |> Expect.equal (Just ( 0, 1 ))
        , test "clearing drops the draft and the result" <|
            \_ ->
                type_ [ ( HomeSide, "3" ), ( AwaySide, "1" ), ( HomeSide, "x" ) ] ( init, fresh )
                    |> (\( model, state ) -> update (Clear 0 2) model state)
                    |> Tuple.mapBoth (.drafts >> Dict.toList) firstResult
                    |> Expect.equal ( [], Nothing )
        , test "the next round starts once every match has a result" <|
            \_ ->
                ( init, fresh )
                    |> (\( model, state ) -> update (SetOutcome 0 2 ( 1, 0 )) model state)
                    |> (\( model, state ) -> update NextRound model state)
                    |> Tuple.second
                    |> .game
                    |> .rounds
                    |> List.length
                    |> Expect.equal 1
        , test "the next round drops all drafts" <|
            \_ ->
                type_ [ ( HomeSide, "3" ) ] ( init, fresh )
                    |> (\( model, state ) -> update (SetOutcome 1 3 ( 1, 0 )) model state)
                    |> (\( model, state ) -> update (SetOutcome 0 2 ( 1, 0 )) model state)
                    |> (\( model, state ) -> update NextRound model state)
                    |> Tuple.mapBoth (.drafts >> Dict.size) (.game >> .rounds >> List.length)
                    |> Expect.equal ( 0, 2 )
        , test "toggling a round expands and collapses it" <|
            \_ ->
                ( init, fresh )
                    |> (\( model, state ) -> update (ToggleRound 1) model state)
                    |> (\( model, state ) ->
                            ( Set.toList model.expanded
                            , update (ToggleRound 1) model state |> Tuple.first |> .expanded |> Set.toList
                            )
                       )
                    |> Expect.equal ( [ 1 ], [] )
        ]
