module Bracket.Stepladder.ViewTest exposing (suite)

import Bracket.Stepladder as Stepladder exposing (Msg(..), State)
import Bracket.Stepladder.View exposing (view)
import Dict
import Expect
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, classes, text)


{-| Seeds 0 (top) to 3 (bottom). 2 beat 3 in the first step.
-}
state : State
state =
    { settings = Stepladder.defaultSettings
    , game =
        Stepladder.start [ 0, 1, 2, 3 ]
            |> Stepladder.update Stepladder.defaultSettings (WinGame True 0)
    }


names : Dict.Dict Int String
names =
    Dict.fromList [ ( 0, "Anna" ), ( 1, "Ben" ), ( 2, "Carla" ), ( 3, "David" ) ]


render : Bool -> Query.Single Msg
render canEdit =
    view { names = names, canEdit = canEdit } state
        |> Query.fromHtml


suite : Test
suite =
    describe "Bracket.Stepladder.View"
        [ test "the final is at the top" <|
            \_ ->
                render True
                    |> Query.findAll [ class "stepladder-title" ]
                    |> Query.first
                    |> Query.has [ text "Final" ]
        , test "the current step is highlighted" <|
            \_ ->
                render True
                    |> Query.find [ classes [ "stepladder-step", "current" ] ]
                    |> Query.has [ text "Step 2", text "Carla", text "Ben" ]
        , test "an editor adds a game to the current step" <|
            \_ ->
                render True
                    |> Query.find [ classes [ "stepladder-step", "current" ] ]
                    |> Query.findAll [ classes [ "game-match-winner", "enabled" ] ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect (WinGame True 1)
        , test "an editor can undo the last decided step" <|
            \_ ->
                render True
                    |> Query.find [ class "game-match-undo" ]
                    |> Event.simulate Event.click
                    |> Event.expect (Undo 0)
        , test "a viewer gets neither winner nor undo buttons" <|
            \_ ->
                render False
                    |> Expect.all
                        [ Query.findAll [ classes [ "game-match-winner", "enabled" ] ] >> Query.count (Expect.equal 0)
                        , Query.findAll [ class "game-match-undo" ] >> Query.count (Expect.equal 0)
                        ]
        ]
