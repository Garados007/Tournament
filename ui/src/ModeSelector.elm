module ModeSelector exposing (Model, Msg(..), init, update, view)

{-| Chooses the mode and the settings of the next tournament.
-}

import Bracket.BestOf as BestOf exposing (BestOf)
import Bracket.Knockout exposing (Places(..))
import Bracket.RoundRobin exposing (Matches(..), Ranking(..))
import Bracket.Scoring exposing (Scoring(..))
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Svg exposing (Svg)
import Svg.Attributes as SA
import Tournament exposing (Mode(..), ModeSettings(..), Order(..), Settings)


type alias Model =
    { help : Bool
    }


type Msg
    = SelectMode Mode
    | SetModeSettings ModeSettings
    | SetOrder Order
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
    , { mode = RoundRobinMode
      , name = "Round robin"
      , description = "Every player plays against every other player. The table decides the places."
      , icon = icon "M3 3H21V21H3ZM9 3V21M15 3V21M3 9H21M3 15H21M4 4L8 8M10 10L14 14M16 16L20 20"
      }
    , { mode = SwissMode
      , name = "Swiss"
      , description = "A fixed number of rounds. In every round players with similar results play each other, but never twice against the same opponent."
      , icon = icon "M3 3H21V21H3ZM10 6H14V10H18V14H14V18H10V14H6V10H10Z"
      }
    , { mode = StepladderMode
      , name = "Stepladder"
      , description = "The two lowest seeds play first. The winner plays the next higher seed, and so on, until the final against the top seed. Meant for seeded players."
      , icon = icon "M3 21H21V3H16V8H11V13H6V18H3Z"
      }
    , { mode = DoubleEliminationMode
      , name = "Double elimination"
      , description = "Players drop out after their second loss. The first loss moves them to the loser bracket. The winners of both brackets meet in the grand final."
      , icon = icon "M2 2H8V8H2M8 5H15M4 8V14M4 14H10V20H4M10 17H15M15 5V17M15 11H22"
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


places : Choice Places
places =
    { label = "Places"
    , options =
        [ { value = Playoff
          , name = "Play out 3rd"
          , description = "The losers of the semi-finals play a match for the 3rd place."
          }
        , { value = Shared
          , name = "Shared 3rd"
          , description = "Both losers of the semi-finals get the 3rd place."
          }
        , { value = NoThirdPlace
          , name = "Only 1st and 2nd"
          , description = "Only the 1st and 2nd place are awarded."
          }
        , { value = PlayAll
          , name = "Play out all"
          , description = "Every player gets a place. The losers of every round play their own knockout for the places they can still reach, e.g. the losers of the quarter-finals for the places 5 to 8."
          }
        ]
    }


reset : Choice Bool
reset =
    { label = "Reset"
    , options =
        [ { value = True
          , name = "On"
          , description = "If the winner of the loser bracket wins the grand final, both players have one loss, and a second match decides."
          }
        , { value = False
          , name = "Off"
          , description = "The grand final decides the tournament."
          }
        ]
    }


bestOfOption : BestOf -> Option BestOf
bestOfOption length =
    { value = length
    , name = String.fromInt length
    , description =
        if length == 1 then
            "A single game decides the match."

        else
            "The first player to win "
                ++ String.fromInt (BestOf.neededWins length)
                ++ " of up to "
                ++ String.fromInt length
                ++ " games wins the match."
    }


bestOf : Choice BestOf
bestOf =
    { label = "Best of"
    , options = List.map bestOfOption BestOf.lengths
    }


finalBestOf : Choice (Maybe BestOf)
finalBestOf =
    { label = "Final: best of"
    , options =
        { value = Nothing
        , name = "Same"
        , description = "The final is as long as the other matches."
        }
            :: List.map
                (\length ->
                    let
                        option =
                            bestOfOption length
                    in
                    { value = Just option.value
                    , name = option.name
                    , description = option.description
                    }
                )
                BestOf.lengths
    }


matches : Choice Matches
matches =
    { label = "Matches"
    , options =
        [ { value = Single
          , name = "Single"
          , description = "Every pair of players plays once."
          }
        , { value = Rematch
          , name = "Rematch"
          , description = "Every pair of players plays twice."
          }
        ]
    }


scoring : Choice Scoring
scoring =
    { label = "Scoring"
    , options =
        [ { value = Points
          , name = "Points"
          , description = "Both players get a score. Equal scores are a draw."
          }
        , { value = WinnerOnly
          , name = "Winner only"
          , description = "Only the winner or a draw is entered."
          }
        ]
    }


ranking : Choice Ranking
ranking =
    { label = "Ranking"
    , options =
        [ { value = ByResults
          , name = "By results"
          , description = "A win gives 3 match points, a draw 1. Equal match points are decided by the score difference, the points scored, the matches among the tied players and the number of wins."
          }
        , { value = ByTotalPoints
          , name = "By total points"
          , description = "The most points scored rank first. Equal points are decided by fewer points conceded and the matches among the tied players."
          }
        ]
    }


rounds : Choice (Maybe Int)
rounds =
    { label = "Rounds"
    , options =
        { value = Nothing
        , name = "Auto"
        , description = "As many rounds as needed until only one player can have won every match: 3 rounds for up to 8 players, 4 for up to 16."
        }
            :: List.map
                (\count ->
                    { value = Just count
                    , name = String.fromInt count
                    , description = String.fromInt count ++ " rounds, but at most one less than the number of players."
                    }
                )
                (List.range 2 7)
    }


order : Choice Order
order =
    { label = "Order"
    , options =
        [ { value = Shuffled
          , name = "Random"
          , description = "The players start in a random order."
          }
        , { value = Seeded
          , name = "Seeded"
          , description = "The players start in the order of the list, the strongest first. In a knockout and a double elimination the best seeds meet as late as possible and skip the pre-round. In a round robin the list sets the order of the table. In a Swiss tournament the top half plays against the bottom half in the first round. In a stepladder the first player waits at the top for the final and the last player starts at the bottom."
          }
        ]
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
            [ choiceKnob bestOf knockout.bestOf <|
                \value -> KnockoutSettings { knockout | bestOf = value }
            , choiceKnob finalBestOf knockout.finalBestOf <|
                \value -> KnockoutSettings { knockout | finalBestOf = value }
            , choiceKnob places knockout.places <|
                \value -> KnockoutSettings { knockout | places = value }
            ]

        RoundRobinSettings roundRobin ->
            [ choiceKnob matches roundRobin.matches <|
                \value -> RoundRobinSettings { roundRobin | matches = value }
            , choiceKnob scoring roundRobin.scoring <|
                \value -> RoundRobinSettings { roundRobin | scoring = value }
            ]
                ++ (if roundRobin.scoring == Points then
                        [ choiceKnob ranking roundRobin.ranking <|
                            \value -> RoundRobinSettings { roundRobin | ranking = value }
                        ]

                    else
                        []
                   )

        SwissSettings swiss ->
            [ choiceKnob rounds swiss.rounds <|
                \value -> SwissSettings { swiss | rounds = value }
            , choiceKnob scoring swiss.scoring <|
                \value -> SwissSettings { swiss | scoring = value }
            ]

        StepladderSettings stepladder ->
            [ choiceKnob bestOf stepladder.bestOf <|
                \value -> StepladderSettings { stepladder | bestOf = value }
            , choiceKnob finalBestOf stepladder.finalBestOf <|
                \value -> StepladderSettings { stepladder | finalBestOf = value }
            ]

        DoubleEliminationSettings doubleElimination ->
            [ choiceKnob bestOf doubleElimination.bestOf <|
                \value -> DoubleEliminationSettings { doubleElimination | bestOf = value }
            , choiceKnob finalBestOf doubleElimination.finalBestOf <|
                \value -> DoubleEliminationSettings { doubleElimination | finalBestOf = value }
            , choiceKnob reset doubleElimination.reset <|
                \value -> DoubleEliminationSettings { doubleElimination | reset = value }
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

        SetOrder value ->
            ( model, { settings | order = value } )

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
                ++ [ choiceView order settings.order SetOrder
                   , Html.button
                        [ class "mode-help-button"
                        , HA.title "What do these settings do?"
                        , HA.attribute "aria-expanded" <| boolString model.help
                        , HE.onClick <| ShowHelp <| not model.help
                        ]
                        [ text "?" ]
                   , viewHint settings
                   ]
        ]


{-| A stepladder is meant to be seeded, but a random order is allowed.
-}
viewHint : Settings -> Html msg
viewHint settings =
    case ( settings.mode, settings.order ) of
        ( StepladderSettings _, Shuffled ) ->
            div [ class "mode-hint" ]
                [ text "A stepladder is meant to be seeded. With a random order, chance decides who waits at the top." ]

        ( StepladderSettings _, Seeded ) ->
            div [ class "mode-hint" ]
                [ text "The first player of the list waits at the top." ]

        _ ->
            text ""


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
    { view = choiceView choice selected toSettings
    , help = choiceHelp choice
    }


choiceView : Choice a -> a -> (a -> msg) -> Html msg
choiceView choice selected toMsg =
    div [ class "mode-setting" ]
        [ text choice.label
        , div [ class "mode-choice" ] <|
            List.map
                (\option ->
                    Html.button
                        [ HA.classList [ ( "selected", option.value == selected ) ]
                        , pressed <| option.value == selected
                        , HA.title option.description
                        , HE.onClick <| toMsg option.value
                        ]
                        [ text option.name ]
                )
                choice.options
        ]


choiceHelp : Choice a -> Html msg
choiceHelp choice =
    helpEntry choice.label <|
        Html.dl [] <|
            List.concatMap
                (\option ->
                    [ Html.dt [] [ text option.name ]
                    , Html.dd [] [ text option.description ]
                    ]
                )
                choice.options


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
            ++ [ choiceHelp order ]
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
