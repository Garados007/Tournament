module Bracket.DoubleElimination.ViewTest exposing (suite)

import Bracket.DoubleElimination as DE exposing (State)
import Bracket.DoubleElimination.View as View exposing (view)
import Bracket.Knockout exposing (Game, Msg(..))
import Dict
import Expect
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, classes, containing, style, text)


names : Dict.Dict Int String
names =
    Dict.fromList (List.map (\id -> ( id, "P" ++ String.fromInt id )) (List.range 0 7))


game : Int -> Game
game count =
    DE.start DE.defaultSettings (List.range 0 (count - 1))


win : Bool -> ( Int, Int ) -> Game -> Game
win isPlayer2 key =
    DE.update DE.defaultSettings (WinGame isPlayer2 key)


{-| 4 players: the winner bracket goes to player 0, the loser bracket to 2.
-}
beforeGrandFinal : Game
beforeGrandFinal =
    game 4
        |> win False ( 2, 0 )
        |> win False ( 2, 1 )
        |> win False ( 1, 0 )
        |> win False ( -3, 0 )
        |> win True ( -2, 0 )


render : { canEdit : Bool, hideFinished : Bool, highlight : Maybe Int } -> Game -> Query.Single Msg
render config g =
    view
        { names = names
        , canEdit = config.canEdit
        , hideFinished = config.hideFinished
        , highlight = config.highlight
        }
        (State DE.defaultSettings g)
        |> Query.fromHtml


viewer : Game -> Query.Single Msg
viewer =
    render { canEdit = False, hideFinished = False, highlight = Nothing }


columnOf : Game -> ( Int, Int ) -> Maybe Int
columnOf g key =
    View.layout g
        |> Maybe.map (\l -> (View.position l key).column)


suite : Test
suite =
    describe "Bracket.DoubleElimination.View"
        [ test "8 players: the losers of every winner bracket round drop into the next column" <|
            \_ ->
                List.map (columnOf (game 8)) [ ( 3, 0 ), ( 2, 0 ), ( 1, 0 ), ( -5, 0 ), ( -4, 0 ), ( -3, 0 ), ( -2, 0 ), DE.grandFinal, DE.resetMatch ]
                    |> Expect.equal (List.map Just [ 0, 1, 3, 1, 2, 3, 4, 5, 6 ])
        , test "16 players: the grand final follows the loser bracket final" <|
            \_ ->
                List.map (columnOf (game 16)) [ ( 1, 0 ), ( -2, 0 ), DE.grandFinal ]
                    |> Expect.equal [ Just 5, Just 6, Just 7 ]
        , describe "every loser bracket match is right of the matches it receives players from" <|
            List.map
                (\count ->
                    test (String.fromInt count) <|
                        \_ ->
                            let
                                g =
                                    game count

                                column key =
                                    columnOf g key |> Maybe.withDefault -1

                                columnOfSlot ( round, index ) =
                                    column ( round, index // 2 )
                            in
                            Bracket.Knockout.keyedMatches g
                                |> List.concatMap
                                    (\( key, match ) ->
                                        List.filterMap identity [ match.winner, match.loser ]
                                            |> List.filter (\( round, _ ) -> round /= Bracket.Knockout.placementRound)
                                            |> List.map (\slot -> ( key, slot ))
                                    )
                                |> List.filter (\( key, slot ) -> columnOfSlot slot <= column key)
                                |> Expect.equal []
                )
                (List.range 3 17)
        , describe "without byes every winner bracket round is next to the loser bracket round that receives its losers" <|
            List.map
                (\count ->
                    test (String.fromInt count) <|
                        \_ ->
                            let
                                g =
                                    game count
                            in
                            Bracket.Knockout.keyedMatches g
                                |> List.filter (\( ( round, _ ), _ ) -> DE.isWinnerRound round)
                                |> List.filterMap (\( key, match ) -> Maybe.map (\slot -> ( key, slot )) match.loser)
                                |> List.filter (\( key, ( round, index ) ) -> columnOf g ( round, index // 2 ) /= Maybe.map ((+) 1) (columnOf g key))
                                |> Expect.equal []
                )
                [ 4, 8, 16 ]
        , test "the loser bracket is below the winner bracket" <|
            \_ ->
                View.layout (game 8)
                    |> Maybe.map
                        (\l ->
                            ( (View.position l ( 3, 3 )).center < (View.position l ( -5, 0 )).center
                            , (View.position l ( -5, 0 )).center < l.rows
                            )
                        )
                    |> Expect.equal (Just ( True, True ))
        , test "the grand final is between both bracket finals" <|
            \_ ->
                View.layout (game 8)
                    |> Maybe.map
                        (\l ->
                            let
                                center key =
                                    (View.position l key).center
                            in
                            ( center ( 1, 0 ) < center DE.grandFinal, center DE.grandFinal < center ( -2, 0 ) )
                        )
                    |> Expect.equal (Just ( True, True ))
        , test "the rounds are named after their bracket" <|
            \_ ->
                viewer (game 8)
                    |> Query.has [ text "WB Round 1", text "LB Round 4", text "Grand final", text "Reset" ]
        , test "empty loser bracket slots tell where their player comes from" <|
            \_ ->
                viewer (game 8)
                    |> Query.findAll [ class "game-match-player-source" ]
                    |> Query.keep (text "Loser WB R1 M3")
                    |> Query.count (Expect.equal 1)
        , test "a slot of a one-match round has no match number" <|
            \_ ->
                viewer (game 8)
                    |> Query.has [ text "Loser WB R3", text "Winner LB R3" ]
        , test "the loser bracket title is above its first match" <|
            \_ ->
                -- seeded, 6 players have no match in the first loser bracket round
                viewer (DE.startSeeded DE.defaultSettings (List.range 0 5))
                    |> Query.find [ class "double-elimination-label", containing [ text "Loser bracket" ] ]
                    |> Query.has [ style "grid-column" "5 / -1" ]
        , test "the reset match waits with a dashed outline" <|
            \_ ->
                viewer beforeGrandFinal
                    |> Query.findAll [ classes [ "game-section-wrapper", "inactive" ] ]
                    |> Query.count (Expect.equal 1)
        , test "the reset match is active once the loser bracket winner won the grand final" <|
            \_ ->
                viewer (win True DE.grandFinal beforeGrandFinal)
                    |> Query.findAll [ classes [ "game-section-wrapper", "inactive" ] ]
                    |> Query.count (Expect.equal 0)
        , test "the reset match is hidden once the grand final decided the tournament" <|
            \_ ->
                viewer (win False DE.grandFinal beforeGrandFinal)
                    |> Query.findAll [ class "game-section-title", containing [ text "Reset" ] ]
                    |> Query.count (Expect.equal 0)
        , test "the line to the reset match is dashed while it isn't needed" <|
            \_ ->
                viewer beforeGrandFinal
                    |> Query.findAll [ classes [ "double-elimination-line", "dashed" ] ]
                    |> Query.count (Expect.equal 1)
        , test "the line to the reset match is solid once it is needed" <|
            \_ ->
                viewer (win True DE.grandFinal beforeGrandFinal)
                    |> Query.findAll [ classes [ "double-elimination-line", "dashed" ] ]
                    |> Query.count (Expect.equal 0)
        , test "no line leads from the winner bracket to the loser bracket without a highlight" <|
            \_ ->
                viewer beforeGrandFinal
                    |> Query.findAll [ classes [ "double-elimination-line", "drop" ] ]
                    |> Query.count (Expect.equal 0)
        , test "the highlight shows the line of the player's drop" <|
            \_ ->
                render { canEdit = False, hideFinished = False, highlight = Just 1 } beforeGrandFinal
                    |> Query.findAll [ classes [ "double-elimination-line", "drop", "highlight", "dashed" ] ]
                    |> Query.count (Expect.equal 1)
        , test "hiding finished matches hides them in both brackets" <|
            \_ ->
                render { canEdit = False, hideFinished = True, highlight = Nothing } beforeGrandFinal
                    |> Query.findAll [ class "game-section" ]
                    |> Query.count (Expect.equal 2)
        , test "the skipped reset match takes no games" <|
            \_ ->
                render { canEdit = True, hideFinished = False, highlight = Nothing } beforeGrandFinal
                    |> Query.findAll [ classes [ "game-match-winner", "enabled" ] ]
                    |> Query.count (Expect.equal 2)
        , test "an editor plays the grand final" <|
            \_ ->
                render { canEdit = True, hideFinished = False, highlight = Nothing } beforeGrandFinal
                    |> Query.findAll [ classes [ "game-match-winner", "enabled" ] ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect (WinGame False DE.grandFinal)
        , test "viewers see no undo buttons" <|
            \_ ->
                viewer beforeGrandFinal
                    |> Query.findAll [ class "game-match-undo" ]
                    |> Query.count (Expect.equal 0)
        ]
