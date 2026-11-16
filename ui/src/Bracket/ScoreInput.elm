module Bracket.ScoreInput exposing (Config, outcomeClass, parse, readOnly, texts, view)

{-| The result input of a match in the modes with a table: two score inputs
with `Points`, the three-way control with `WinnerOnly`. The element around it
needs the class `score-host`, so the clear button can be placed.
-}

import Bracket.Scoring as Scoring exposing (Cell(..), Outcome(..), Scoring(..))
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE


{-| `cell`, `texts` and `names` are from the view of the first player. `draft`
is whether `texts` aren't a complete result yet.
-}
type alias Config msg =
    { scoring : Scoring
    , cell : Cell
    , texts : ( String, String )
    , draft : Bool
    , names : ( String, String )
    , onFirst : String -> msg
    , onSecond : String -> msg
    , onOutcome : ( Int, Int ) -> msg
    , onClear : msg
    }


{-| The texts of the score inputs: the draft if there is one, else the result.
-}
texts : Maybe ( String, String ) -> Cell -> ( String, String )
texts draft c =
    case ( draft, c ) of
        ( Just value, _ ) ->
            value

        ( Nothing, Played ( a, b ) ) ->
            ( String.fromInt a, String.fromInt b )

        _ ->
            ( "", "" )


{-| A complete result if both texts are numbers.
-}
parse : ( String, String ) -> Maybe ( Int, Int )
parse ( a, b ) =
    Maybe.map2 Tuple.pair
        (String.toInt <| String.trim a)
        (String.toInt <| String.trim b)


view : Config msg -> List (Html msg)
view config =
    let
        ( firstName, secondName ) =
            config.names

        clear =
            if config.draft || config.cell /= Open then
                Html.button
                    [ class "score-clear"
                    , HA.title "Clear the result"
                    , HE.onClick config.onClear
                    ]
                    []

            else
                text ""
    in
    case config.scoring of
        Points ->
            let
                ( a, b ) =
                    config.texts

                input onInput value title =
                    Html.input
                        [ class "score-field"
                        , HA.value value
                        , HA.title title
                        , HA.attribute "aria-label" title
                        , HA.attribute "autocomplete" "off"
                        , HE.onInput onInput
                        ]
                        []
            in
            [ div [ HA.classList [ ( "score-pair", True ), ( "draft", config.draft ) ] ]
                [ input config.onFirst a <| "Score of " ++ firstName
                , text ":"
                , input config.onSecond b <| "Score of " ++ secondName
                ]
            , clear
            ]

        WinnerOnly ->
            [ div [ class "score-outcome" ] <|
                List.map
                    (\( score, letter, title ) ->
                        Html.button
                            [ HA.classList [ ( "selected", config.cell == Played score ) ]
                            , HA.attribute "aria-pressed" <|
                                if config.cell == Played score then
                                    "true"

                                else
                                    "false"
                            , HA.title title
                            , HE.onClick <| config.onOutcome score
                            ]
                            [ text letter ]
                    )
                    [ ( ( 1, 0 ), "W", firstName ++ " wins" )
                    , ( ( 0, 0 ), "D", "Draw" )
                    , ( ( 0, 1 ), "L", secondName ++ " wins" )
                    ]
            , clear
            ]


{-| The result as text for viewers and finished matches.
-}
readOnly : Scoring -> Cell -> String
readOnly scoring c =
    case ( c, scoring ) of
        ( Played ( a, b ), Points ) ->
            String.fromInt a ++ " : " ++ String.fromInt b

        ( Played _, WinnerOnly ) ->
            case Scoring.cellOutcome c of
                Just Win ->
                    "W"

                Just Draw ->
                    "D"

                Just Loss ->
                    "L"

                Nothing ->
                    ""

        ( Forfeit _, _ ) ->
            "w/o"

        ( Cancelled, _ ) ->
            "–"

        ( Open, _ ) ->
            ""


{-| The class that colors a match by its outcome for the first player.
-}
outcomeClass : Cell -> String
outcomeClass c =
    case ( c, Scoring.cellOutcome c ) of
        ( _, Just Win ) ->
            "win"

        ( _, Just Draw ) ->
            "draw"

        ( _, Just Loss ) ->
            "loss"

        ( Cancelled, Nothing ) ->
            "cancelled"

        ( _, Nothing ) ->
            "open"
