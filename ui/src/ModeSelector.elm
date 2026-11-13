module ModeSelector exposing (Model, Msg(..), init, update, view)

{-| Chooses the mode and the settings of the next tournament.
-}

import Bracket.Knockout exposing (ThirdPlace(..))
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Svg exposing (Svg)
import Svg.Attributes as SA
import Tournament exposing (Mode(..), ModeSettings(..), Settings)


type alias Model =
    { help : Bool
    }


type Msg
    = SelectMode Mode
    | SetModeSettings ModeSettings
    | SetShuffle Bool
    | ShowHelp Bool


init : Model
init =
    { help = False }


type alias Category =
    { mode : Mode
    , name : String
    , description : String
    , icon : Svg Never
    }


categories : List Category
categories =
    [ { mode = KnockoutMode
      , name = "Knockout"
      , description = "Players drop out after their first loss. The last one left wins."
      , icon = icon "M2 3H8V9H2M8 6H14M2 15H8V21H2M8 18H14M14 6V18M14 12H22"
      }
    ]


type alias Option a =
    { value : a
    , name : String
    , description : String
    }


type alias Choice a =
    { label : String
    , options : List (Option a)
    }


thirdPlace : Choice ThirdPlace
thirdPlace =
    { label = "3rd place"
    , options =
        [ { value = Playoff
          , name = "Playoff"
          , description = "The losers of the semi-finals play a match for the 3rd place."
          }
        , { value = Shared
          , name = "Shared"
          , description = "Both losers of the semi-finals get the 3rd place."
          }
        , { value = NoThirdPlace
          , name = "None"
          , description = "Only the 1st and 2nd place are awarded."
          }
        ]
    }


shuffle : { label : String, description : String }
shuffle =
    { label = "Shuffle players"
    , description = "The players start in a random order. Otherwise they start in the order they were added."
    }


{-| A setting of a mode with its explanation for the help window.
-}
type alias Knob =
    { view : Html ModeSettings
    , help : Html Never
    }


modeKnobs : ModeSettings -> List Knob
modeKnobs mode =
    case mode of
        KnockoutSettings knockout ->
            [ choiceKnob thirdPlace knockout.thirdPlace <|
                \value -> KnockoutSettings { knockout | thirdPlace = value }
            ]


{-| Selecting the current mode again keeps its settings; another mode starts
with its defaults.
-}
update : Msg -> Model -> Settings -> ( Model, Settings )
update msg model settings =
    case msg of
        SelectMode mode ->
            ( model
            , if Tournament.modeOf settings.mode == mode then
                settings

              else
                { settings | mode = Tournament.modeDefaults mode }
            )

        SetModeSettings mode ->
            ( model, { settings | mode = mode } )

        SetShuffle value ->
            ( model, { settings | shuffle = value } )

        ShowHelp help ->
            ( { model | help = help }, settings )


view : Model -> Settings -> Html Msg
view model settings =
    let
        selected =
            Tournament.modeOf settings.mode

        knobs =
            modeKnobs settings.mode
    in
    div [ class "mode-selector" ]
        [ if model.help then
            viewHelp selected knobs

          else
            text ""
        , div [ class "mode-categories" ] <|
            List.map (viewCategory selected) categories
        , div [ class "mode-settings" ] <|
            List.map (.view >> Html.map SetModeSettings) knobs
                ++ [ Html.label
                        [ class "mode-setting"
                        , HA.title shuffle.description
                        ]
                        [ Html.input
                            [ HA.type_ "checkbox"
                            , HA.checked settings.shuffle
                            , HE.onCheck SetShuffle
                            ]
                            []
                        , text shuffle.label
                        ]
                   , Html.button
                        [ class "mode-help-button"
                        , HA.title "What do these settings do?"
                        , HA.attribute "aria-expanded" <| boolString model.help
                        , HE.onClick <| ShowHelp <| not model.help
                        ]
                        [ text "?" ]
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
        , HA.title category.description
        , HE.onClick <| SelectMode category.mode
        ]
        [ Html.map never category.icon
        , div [ class "mode-category-name" ] [ text category.name ]
        ]


choiceKnob : Choice a -> a -> (a -> ModeSettings) -> Knob
choiceKnob choice selected toSettings =
    { view =
        div [ class "mode-setting" ]
            [ text choice.label
            , div [ class "mode-choice" ] <|
                List.map
                    (\option ->
                        Html.button
                            [ HA.classList [ ( "selected", option.value == selected ) ]
                            , pressed <| option.value == selected
                            , HA.title option.description
                            , HE.onClick <| toSettings option.value
                            ]
                            [ text option.name ]
                    )
                    choice.options
            ]
    , help =
        helpEntry choice.label <|
            Html.dl [] <|
                List.concatMap
                    (\option ->
                        [ Html.dt [] [ text option.name ]
                        , Html.dd [] [ text option.description ]
                        ]
                    )
                    choice.options
    }


viewHelp : Mode -> List Knob -> Html Msg
viewHelp selected knobs =
    div [ class "mode-help" ]
        [ Html.button
            [ class "mode-help-close"
            , HA.title "Close"
            , HE.onClick <| ShowHelp False
            ]
            [ text "✕" ]
        , categories
            |> List.filter (\category -> category.mode == selected)
            |> List.map (\category -> helpEntry category.name <| text category.description)
            |> div []
        , List.map .help knobs
            ++ [ helpEntry shuffle.label <| text shuffle.description ]
            |> div []
            |> Html.map never
        ]


helpEntry : String -> Html msg -> Html msg
helpEntry title content =
    div [ class "mode-help-entry" ]
        [ div [ class "mode-help-title" ] [ text title ]
        , content
        ]


pressed : Bool -> Html.Attribute msg
pressed =
    HA.attribute "aria-pressed" << boolString


boolString : Bool -> String
boolString value =
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
