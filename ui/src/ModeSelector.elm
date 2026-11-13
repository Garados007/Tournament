module ModeSelector exposing (Msg(..), update, view)

{-| Chooses the mode and the settings of the next tournament.
-}

import Bracket.Knockout exposing (ThirdPlace(..))
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Svg exposing (Svg)
import Svg.Attributes as SA
import Tournament exposing (Mode(..), ModeSettings(..), Settings)


type Msg
    = SelectMode Mode
    | SetModeSettings ModeSettings
    | SetShuffle Bool


type alias Category =
    { mode : Mode
    , name : String
    , icon : Svg Never
    }


categories : List Category
categories =
    [ { mode = KnockoutMode
      , name = "Knockout"
      , icon = icon "M2 3H8V9H2M8 6H14M2 15H8V21H2M8 18H14M14 6V18M14 12H22"
      }
    ]


{-| Selecting the current mode again keeps its settings; another mode starts
with its defaults.
-}
update : Msg -> Settings -> Settings
update msg settings =
    case msg of
        SelectMode mode ->
            if Tournament.modeOf settings.mode == mode then
                settings

            else
                { settings | mode = Tournament.modeDefaults mode }

        SetModeSettings mode ->
            { settings | mode = mode }

        SetShuffle shuffle ->
            { settings | shuffle = shuffle }


view : Settings -> Html Msg
view settings =
    div [ class "mode-selector" ]
        [ div [ class "mode-categories" ] <|
            List.map
                (viewCategory <| Tournament.modeOf settings.mode)
                categories
        , div [ class "mode-settings" ]
            [ Html.map SetModeSettings <| viewModeSettings settings.mode
            , checkbox "Shuffle players" settings.shuffle SetShuffle
            ]
        ]


viewCategory : Mode -> Category -> Html Msg
viewCategory selected category =
    Html.button
        [ HA.classList
            [ ( "mode-category", True )
            , ( "selected", category.mode == selected )
            ]
        , pressed <| category.mode == selected
        , HE.onClick <| SelectMode category.mode
        ]
        [ Html.map never category.icon
        , div [ class "mode-category-name" ] [ text category.name ]
        ]


viewModeSettings : ModeSettings -> Html ModeSettings
viewModeSettings mode =
    case mode of
        KnockoutSettings knockout ->
            choice "3rd place"
                knockout.thirdPlace
                [ ( Playoff, "Playoff" )
                , ( Shared, "Shared" )
                , ( NoThirdPlace, "None" )
                ]
                |> Html.map (\thirdPlace -> KnockoutSettings { knockout | thirdPlace = thirdPlace })


choice : String -> a -> List ( a, String ) -> Html a
choice label selected options =
    div [ class "mode-setting" ]
        [ text label
        , div [ class "mode-choice" ] <|
            List.map
                (\( value, name ) ->
                    Html.button
                        [ HA.classList [ ( "selected", value == selected ) ]
                        , pressed <| value == selected
                        , HE.onClick value
                        ]
                        [ text name ]
                )
                options
        ]


checkbox : String -> Bool -> (Bool -> msg) -> Html msg
checkbox label checked toMsg =
    Html.label [ class "mode-setting" ]
        [ Html.input
            [ HA.type_ "checkbox"
            , HA.checked checked
            , HE.onCheck toMsg
            ]
            []
        , text label
        ]


pressed : Bool -> Html.Attribute msg
pressed value =
    HA.attribute "aria-pressed" <|
        if value then
            "true"

        else
            "false"


{-| A line drawing in the text color.
-}
icon : String -> Svg msg
icon path =
    Svg.svg
        [ SA.class "mode-icon"
        , SA.viewBox "0 0 24 24"
        ]
        [ Svg.path
            [ SA.d path
            , SA.fill "none"
            , SA.stroke "currentColor"
            , SA.strokeWidth "2"
            , SA.strokeLinecap "round"
            , SA.strokeLinejoin "round"
            ]
            []
        ]
