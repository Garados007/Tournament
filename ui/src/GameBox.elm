module GameBox exposing (Controls, Headers(..), Model, Msg, init, update, view)

import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE


{-| What the column headers of a table show.
-}
type Headers
    = Names
    | NamesAndNumbers
    | Numbers


type alias Model =
    { scale : Int
    , hide : Bool
    , headers : Headers
    }


type Msg
    = SetScale Int
    | SetHide Bool
    | SetHeaders Headers


{-| The controls besides the zoom that the mode uses.
-}
type alias Controls =
    { hideFinished : Bool
    , headers : Bool
    }


init : Model
init =
    { scale = 0
    , hide = False
    , headers = Names
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetScale scale ->
            { model | scale = scale }

        SetHide hide ->
            { model | hide = hide }

        SetHeaders headers ->
            { model | headers = headers }


nextHeaders : Headers -> Headers
nextHeaders headers =
    case headers of
        Names ->
            NamesAndNumbers

        NamesAndNumbers ->
            Numbers

        Numbers ->
            Names


{-| The last argument renders the tournament with the settings of the box.
-}
view : (Msg -> msg) -> Controls -> (Model -> Html msg) -> Model -> Html msg
view toMsg controls content model =
    div [ class "game-box" ]
        [ Html.map toMsg <| viewControls controls model
        , div
            [ class "game-box-scaler"
            , HA.style "font-size" <|
                String.fromFloat (0.75 ^ toFloat model.scale)
                    ++ "em"
            ]
            [ content model ]
        ]


viewControls : Controls -> Model -> Html Msg
viewControls controls model =
    div [ class "game-box-controls" ] <|
        List.concat
            [ if controls.hideFinished then
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
                ]

              else
                []
            , if controls.headers then
                [ Html.button
                    [ HE.onClick <| SetHeaders <| nextHeaders model.headers
                    , HA.title <|
                        case nextHeaders model.headers of
                            Names ->
                                "Show names in the column headers"

                            NamesAndNumbers ->
                                "Show names and numbers in the column headers"

                            Numbers ->
                                "Show numbers in the column headers"
                    ]
                    [ text <|
                        case model.headers of
                            Names ->
                                "A"

                            NamesAndNumbers ->
                                "1A"

                            Numbers ->
                                "1"
                    ]
                ]

              else
                []
            , [ Html.button
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
            ]
