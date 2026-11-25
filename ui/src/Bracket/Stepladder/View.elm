module Bracket.Stepladder.View exposing (view)

{-| The ladder from the top seed at the top down to the first match.
-}

import Bracket.BestOf as BestOf exposing (BestOf)
import Bracket.Stepladder as Stepladder exposing (Msg(..), State, Step)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import List.Extra
import PlayerId exposing (PlayerId)


type alias Config =
    { names : Dict PlayerId String
    , canEdit : Bool
    }


{-| The higher steps move to the right like a staircase, but never by more
than 8em in total.
-}
view : Config -> State -> Html Msg
view config state =
    let
        steps : List Step
        steps =
            Stepladder.steps state.settings state.game

        count : Int
        count =
            List.length steps

        rise : Float
        rise =
            min 1.5 (8 / toFloat (max 1 (count - 1)))

        indent : Int -> Html.Attribute msg
        indent index =
            HA.style "margin-left" <| String.fromFloat (toFloat index * rise) ++ "em"
    in
    div [ class "stepladder" ] <|
        List.reverse <|
            List.map
                (\step ->
                    div [ class "stepladder-step-wrapper", indent step.index ]
                        [ viewStep config state count step ]
                )
                steps


{-| The seed is the position in the ladder counted from the top.
-}
seedOf : State -> PlayerId -> Maybe Int
seedOf state id =
    List.Extra.elemIndex id state.game.ladder
        |> Maybe.map (\index -> List.length state.game.ladder - index)


viewStep : Config -> State -> Int -> Step -> Html Msg
viewStep config state count step =
    let
        bestOf : BestOf
        bestOf =
            Stepladder.matchLength state.settings state.game step.index

        isCurrent : Bool
        isCurrent =
            Stepladder.currentIndex state.settings state.game == Just step.index
    in
    div
        [ HA.classList
            [ ( "stepladder-step", True )
            , ( "current", isCurrent )
            , ( "decided", step.winner /= Nothing )
            ]
        ]
        [ div [ class "stepladder-head" ]
            [ div [ class "stepladder-title" ] <|
                List.map (\part -> Html.span [] [ text part ]) <|
                    List.concat
                        [ [ Stepladder.stepName state.game step.index ]
                        , if bestOf > 1 then
                            [ "Best of " ++ String.fromInt bestOf ]

                          else
                            []
                        , if step.match.walkover /= Nothing then
                            [ "Walkover" ]

                          else
                            []
                        ]

            -- the winner climbs from this step up to the box of the next step
            , if step.index < count - 1 then
                div
                    [ HA.classList
                        [ ( "stepladder-climb", True )
                        , ( "taken", step.winner /= Nothing )
                        ]
                    ]
                    []

              else
                text ""
            ]
        , div [ class "game-section" ]
            [ viewPlayer config state step bestOf True (Just step.player2)
            , viewPlayer config state step bestOf False step.player1
            , if config.canEdit && Stepladder.canUndo state.settings step.index state.game then
                div
                    [ class "game-match-undo"
                    , HA.title <|
                        if bestOf > 1 then
                            "Undo the last game"

                        else
                            "Undo the result"
                    , HE.onClick <| Undo step.index
                    ]
                    []

              else
                text ""
            ]
        ]


{-| Player 2 waits at the step, player 1 climbs up from the step below.
-}
viewPlayer : Config -> State -> Step -> BestOf -> Bool -> Maybe PlayerId -> Html Msg
viewPlayer config state step bestOf isPlayer2 player =
    let
        gamesWon : Int
        gamesWon =
            BestOf.wins step.match.games
                |> (if isPlayer2 then
                        Tuple.second

                    else
                        Tuple.first
                   )
    in
    div
        [ HA.classList
            [ ( "game-match-player", True )
            , ( "won", step.winner == Just isPlayer2 )
            ]
        ]
    <|
        [ div [ class "stepladder-seed" ]
            [ text <|
                case Maybe.andThen (seedOf state) player of
                    Just seed ->
                        String.fromInt seed

                    Nothing ->
                        ""
            ]
        , div [ class "game-match-player-name" ]
            [ case player of
                Just id ->
                    Html.span
                        [ HA.classList
                            [ ( "player", True )
                            , ( "alive", not <| Stepladder.isEliminated state.settings id state.game )
                            ]
                        ]
                        [ text <| Maybe.withDefault "" <| Dict.get id config.names ]

                Nothing ->
                    Html.span [ class "stepladder-open" ]
                        [ text <| "Winner of step " ++ String.fromInt step.index ]
            ]
        ]
            ++ (if bestOf > 1 then
                    [ div [ class "game-match-wins" ]
                        [ text <|
                            if player == Nothing then
                                ""

                            else
                                String.fromInt gamesWon
                        ]
                    ]

                else
                    []
               )
            ++ [ div
                    [ HA.classList
                        [ ( "game-match-winner", True )
                        , ( "add-game", bestOf > 1 )
                        , ( "enabled", config.canEdit && Stepladder.canWin state.settings step.index state.game )
                        ]
                    , HA.title <|
                        if bestOf > 1 then
                            "Add a won game"

                        else
                            "Select as winner"
                    , HE.onClick <| WinGame isPlayer2 step.index
                    ]
                    []
               ]
