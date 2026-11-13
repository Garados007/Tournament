module GameBox exposing (Model, Msg, init, update, view)

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


{-| The second argument renders the tournament. It gets whether to hide the
finished matches.
-}
view : (Msg -> msg) -> (Bool -> Html msg) -> Model -> Html msg
view toMsg content model =
    div [ class "game-box" ]
        [ Html.map toMsg <| viewControls model
        , div
            [ class "game-box-scaler"
            , HA.style "font-size" <|
                String.fromFloat (0.75 ^ toFloat model.scale)
                    ++ "em"
            ]
            [ content model.hide ]
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
