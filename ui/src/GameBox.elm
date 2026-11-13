module GameBox exposing (Model, Msg, init, update, view)

import Bracket.Knockout exposing (Game)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE


type alias Model =
    { scale : Int
    , hide : Bool
    }


type Msg
    = SetScale Int
    | SetHide Bool


init : Model
init =
    { scale = 0
    , hide = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetScale scale ->
            { model | scale = scale }

        SetHide hide ->
            { model | hide = hide }


{-| The second argument embeds the bracket, which sends its messages to the
caller instead of the box.
-}
view : (Msg -> msg) -> (Html Bracket.Knockout.Msg -> Html msg) -> Model -> Game -> Html msg
view toMsg embedGame model game =
    div [ class "game-box" ]
        [ Html.map toMsg <| viewControls model
        , div
            [ class "game-box-scaler"
            , HA.style "font-size" <|
                String.fromFloat (0.75 ^ toFloat model.scale)
                    ++ "em"
            ]
            [ embedGame <|
                Bracket.Knockout.viewGame <|
                    if model.hide then
                        Bracket.Knockout.hideFinishedMatches game

                    else
                        game
            ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    div [ class "game-box-controls" ]
        [ Html.button
            [ HE.onClick <| SetHide <| not model.hide
            , HA.title <|
                if model.hide then
                    "Show all matches again"

                else
                    "Hide finished matches"
            ]
            [ text <|
                if model.hide then
                    "🔶"

                else
                    "🔸"
            ]
        , Html.button
            [ HE.onClick <| SetScale <| model.scale + 1
            , HA.title "Zoom out"
            ]
            [ text "-" ]
        , Html.button
            [ HE.onClick <| SetScale <| model.scale - 1
            , HA.title "Zoom in"
            ]
            [ text "+" ]
        ]
