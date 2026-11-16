module Bracket.Knockout.ViewTest exposing (suite)

import Bracket.Knockout exposing (Game, Msg, Places(..), viewGame)
import Bracket.Knockout.Placement exposing (apply)
import Dict
import Expect
import Helper exposing (knockoutSettings, setWinner, start)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, classes, containing, style, text)


names : Dict.Dict Int String
names =
    Dict.fromList (List.map (\id -> ( id, "P" ++ String.fromInt id )) (List.range 0 15))


startWith : Places -> Int -> Game
startWith places n =
    apply (knockoutSettings places) (start n)


render : { hideFinished : Bool, highlight : Maybe Int } -> Game -> Query.Single Msg
render config game =
    viewGame
        { names = names
        , canEdit = True
        , hideFinished = config.hideFinished
        , settings = knockoutSettings PlayAll
        , highlight = config.highlight
        }
        game
        |> Query.fromHtml


plain : Game -> Query.Single Msg
plain =
    render { hideFinished = False, highlight = Nothing }


labels : Query.Single Msg -> Query.Multiple Msg
labels =
    Query.findAll [ class "knockout-label" ]


suite : Test
suite =
    describe "Bracket.Knockout.viewGame"
        [ test "8 players: the placement brackets follow in the order of their places" <|
            \_ ->
                plain (startWith PlayAll 8)
                    |> labels
                    |> Expect.all
                        [ Query.count (Expect.equal 3)
                        , Query.index 0 >> Query.has [ text "Places 3–4" ]
                        , Query.index 1 >> Query.has [ text "Places 5–8" ]
                        , Query.index 2 >> Query.has [ text "Places 7–8" ]
                        ]
        , test "6 players: the losers of the pre-round play for the places 5 and 6" <|
            \_ ->
                plain (startWith PlayAll 6)
                    |> labels
                    |> Query.index 1
                    |> Query.has [ text "Places 5–6" ]
        , test "the slots of a placement bracket name the match their player comes from" <|
            \_ ->
                plain (startWith PlayAll 8)
                    |> Query.findAll [ class "game-match-player-source" ]
                    |> Expect.all
                        [ Query.count (Expect.equal 8)
                        , Query.index 0 >> Query.has [ text "Loser R2 M1" ]
                        , Query.index 2 >> Query.has [ text "Loser R1 M1" ]
                        , Query.index 6 >> Query.has [ text "Loser 5–8 R1 M1" ]
                        ]
        , test "every placement bracket ends in the column of the final" <|
            \_ ->
                plain (startWith PlayAll 8)
                    |> Query.findAll [ class "game-section-wrapper", style "grid-column-start" "5" ]
                    |> Query.count (Expect.equal 4)
        , test "the highlight follows a player into their placement bracket" <|
            \_ ->
                let
                    game =
                        startWith PlayAll 8 |> setWinner False ( 2, 0 )

                    loser =
                        Dict.get ( -501, 0 ) game.slots
                in
                render { hideFinished = False, highlight = loser } game
                    |> Query.findAll [ classes [ "game-match-player-name", "highlight" ] ]
                    |> Expect.all
                        [ Query.count (Expect.equal 2)
                        , Query.index 1 >> Query.has [ containing [ text "P" ] ]
                        , \_ -> loser |> Expect.notEqual Nothing
                        ]
        , test "a line leads the highlighted player from the main bracket into their placement bracket" <|
            \_ ->
                let
                    game =
                        startWith PlayAll 8 |> setWinner False ( 2, 0 )
                in
                Expect.all
                    [ render { hideFinished = False, highlight = Dict.get ( -501, 0 ) game.slots }
                        >> Query.findAll [ class "knockout-drop" ]
                        >> Query.count (Expect.equal 1)
                    , plain
                        >> Query.findAll [ class "knockout-drop" ]
                        >> Query.count (Expect.equal 0)
                    , render { hideFinished = False, highlight = Dict.get ( 2, 0 ) game.slots }
                        >> Query.findAll [ class "knockout-drop" ]
                        >> Query.count (Expect.equal 0)
                    ]
                    game
        , test "the line follows the player from one placement bracket into the next" <|
            \_ ->
                let
                    game =
                        startWith PlayAll 8
                            |> setWinner False ( 2, 0 )
                            |> setWinner False ( 2, 1 )
                            |> setWinner False ( -501, 0 )
                in
                render { hideFinished = False, highlight = Dict.get ( -700, 0 ) game.slots } game
                    |> Query.findAll [ class "knockout-drop" ]
                    |> Query.count (Expect.equal 2)
        , test "the line leads to a player with a bye into the match of the next round" <|
            \_ ->
                let
                    game =
                        startWith PlayAll 26 |> setWinner False ( 4, 4 )

                    loser =
                        Dict.get ( -1703, 4 ) game.slots
                in
                Expect.all
                    [ \_ -> loser |> Expect.notEqual Nothing
                    , render { hideFinished = False, highlight = loser }
                        >> Query.findAll [ class "knockout-drop" ]
                        >> Query.count (Expect.equal 1)
                    ]
                    game
        , test "no line leads into a hidden match" <|
            \_ ->
                let
                    game =
                        startWith PlayAll 4
                            |> setWinner False ( 1, 0 )
                            |> setWinner False ( 1, 1 )
                            |> setWinner False ( -300, 0 )
                in
                render { hideFinished = True, highlight = Dict.get ( -1, 3 ) game.slots } game
                    |> Query.findAll [ class "knockout-drop" ]
                    |> Query.count (Expect.equal 0)
        , test "a finished placement bracket is hidden with its heading" <|
            \_ ->
                startWith PlayAll 4
                    |> setWinner False ( 1, 0 )
                    |> setWinner False ( 1, 1 )
                    |> setWinner False ( -300, 0 )
                    |> render { hideFinished = True, highlight = Nothing }
                    |> labels
                    |> Query.count (Expect.equal 0)
        , test "a bracket with a third-place match has no headings" <|
            \_ ->
                plain (startWith Playoff 8)
                    |> labels
                    |> Query.count (Expect.equal 0)
        ]
