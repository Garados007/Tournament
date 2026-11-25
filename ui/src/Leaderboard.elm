module Leaderboard exposing
    ( Model
    , Msg(..)
    , init
    , observe
    , subscriptions
    , update
    , view
    , viewToggler
    )

{-| The panel with the podium and the table of all players. Whether it is open
is local to every viewer.
-}

import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Html.Keyed
import Json.Decode as JD
import PlayerId exposing (PlayerId)
import Standings exposing (Row, Standings, Status(..))
import Svg
import Svg.Attributes as SA


{-| `celebrate` is set when the leaderboard opened because the tournament was
just finished. `celebrations` counts these openings, so the animations start
again for every one.
-}
type alias Model =
    { open : Bool
    , celebrate : Bool
    , celebrations : Int
    }


type Msg
    = SetOpen Bool
    | ExportCsv


init : Model
init =
    { open = False
    , celebrate = False
    , celebrations = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetOpen open ->
            { model | open = open, celebrate = False }

        ExportCsv ->
            model


{-| Gets whether the tournament was finished before and after a change,
`Nothing` without a tournament. The end of a tournament opens the leaderboard
with a celebration.
-}
observe : Maybe Bool -> Maybe Bool -> Model -> Model
observe before after model =
    if after == Nothing then
        { model | open = False, celebrate = False }

    else if before == Just False && after == Just True then
        { open = True, celebrate = True, celebrations = model.celebrations + 1 }

    else
        model


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.open then
        Browser.Events.onKeyDown <|
            JD.andThen
                (\key ->
                    if key == "Escape" then
                        JD.succeed <| SetOpen False

                    else
                        JD.fail key
                )
                (JD.field "key" JD.string)

    else
        Sub.none


viewToggler : { finished : Bool } -> Model -> Html Msg
viewToggler { finished } model =
    Html.Keyed.node "div"
        [ HA.classList
            [ ( "leaderboard-toggler", True )
            , ( "finished", finished )
            , ( "open", model.open )
            , ( "pulse", model.celebrate )
            ]
        , HA.title "Leaderboard"
        , HE.onClick <| SetOpen <| not model.open
        ]
        [ ( String.fromInt model.celebrations, trophy ) ]


trophy : Html msg
trophy =
    Svg.svg [ SA.viewBox "0 0 24 24" ]
        [ Svg.path [ SA.d "M7 3h10v5a5 5 0 0 1-10 0z" ] []
        , Svg.path [ SA.class "handles", SA.d "M7 5H4.5v1.5a3 3 0 0 0 3 3M17 5h2.5v1.5a3 3 0 0 1-3 3" ] []
        , Svg.rect [ SA.x "11", SA.y "12.5", SA.width "2", SA.height "5" ] []
        , Svg.path [ SA.d "M7.5 21v-1.5a2 2 0 0 1 2-2h5a2 2 0 0 1 2 2V21z" ] []
        ]


view : Dict PlayerId String -> Standings -> Model -> Html Msg
view names standings model =
    div
        [ HA.classList
            [ ( "layout-leaderboard", True )
            , ( "open", model.open )
            , ( "celebrate", model.celebrate )
            ]
        ]
        [ Html.Keyed.node "div"
            [ class "leaderboard" ]
            [ ( String.fromInt model.celebrations
              , div [ class "leaderboard-content" ]
                    [ div [ class "leaderboard-head" ]
                        [ div [ class "leaderboard-title" ] [ text "Leaderboard" ]
                        , button
                            [ class "leaderboard-csv"
                            , HE.onClick ExportCsv
                            , HA.title "Export the leaderboard as CSV for a spreadsheet program"
                            ]
                            [ text "CSV" ]
                        , button
                            [ class "leaderboard-close"
                            , HE.onClick <| SetOpen False
                            , HA.title "Close"
                            ]
                            [ text "✕" ]
                        ]
                    , if standings.finished then
                        text ""

                      else
                        div [ class "leaderboard-provisional" ]
                            [ text "Provisional: the tournament is still running." ]
                    , viewPodium names standings model
                    , viewTable names standings
                    ]
              )
            ]
        ]


name : Dict PlayerId String -> PlayerId -> String
name names id =
    Maybe.withDefault "" <| Dict.get id names


{-| The steps in the order 2, 1, 3.
-}
viewPodium : Dict PlayerId String -> Standings -> Model -> Html msg
viewPodium names standings model =
    let
        places : List ( Int, List PlayerId )
        places =
            Standings.podium standings

        step : Int -> List (Html msg)
        step place =
            case List.filter (Tuple.first >> (==) place) places of
                ( _, ids ) :: _ ->
                    [ div [ class <| "podium-step place-" ++ String.fromInt place ]
                        [ div [ class "podium-names" ] <|
                            List.map (\id -> div [ class "podium-name" ] [ text <| name names id ]) ids
                        , div [ class "podium-block" ] [ text <| String.fromInt place ]
                        ]
                    ]

                [] ->
                    []
    in
    if List.isEmpty places then
        text ""

    else
        div [ class "podium" ] <|
            step 2
                ++ step 1
                ++ step 3
                ++ (if model.celebrate then
                        [ confetti ]

                    else
                        []
                   )


{-| `HA.style` can't set custom properties.
-}
customProperties : List ( String, String ) -> Html.Attribute msg
customProperties =
    List.map (\( key, value ) -> key ++ ":" ++ value)
        >> String.join ";"
        >> HA.attribute "style"


{-| The pieces fly from the middle of the podium. Their paths, colors and delays
vary by their number.
-}
confetti : Html msg
confetti =
    div [ class "confetti" ] <|
        List.map
            (\i ->
                Html.i
                    [ customProperties
                        [ ( "--x", String.fromFloat (toFloat (modBy 401 (i * i * 37 + i * 113) - 200) / 10) ++ "em" )
                        , ( "--y", String.fromFloat (toFloat (35 + modBy 55 (i * i * 13 + i * 7)) / 10) ++ "em" )
                        , ( "--r", String.fromInt (modBy 13 (i * i + i * 5) * 60 - 360) ++ "deg" )
                        , ( "--delay", String.fromInt (modBy 9 (i * i + i * 4) * 45) ++ "ms" )
                        , ( "background-color"
                          , Maybe.withDefault "#e6b422" <|
                                List.head <|
                                    List.drop (modBy 5 i) [ "#e6b422", "#e05a5a", "#5ab0e0", "#6cc36c", "#c77ddb" ]
                          )
                        ]
                    ]
                    []
            )
            (List.range 0 47)


viewTable : Dict PlayerId String -> Standings -> Html msg
viewTable names standings =
    let
        draws : List a -> List a
        draws values =
            if standings.draws then
                values

            else
                []

        viewRow : Int -> Row -> Html msg
        viewRow index r =
            Html.tr
                [ HA.classList
                    [ ( "still-in", r.status == StillIn )
                    , ( "out", r.status == Out )
                    , ( "removed", r.status == Removed )
                    ]
                , customProperties [ ( "--i", String.fromInt <| min 20 index ) ]
                ]
                ([ Html.td [ class "place" ] [ text <| Maybe.withDefault "–" <| Maybe.map String.fromInt r.place ]
                 , Html.td
                    [ class "name"
                    , HA.title <|
                        if r.status == Removed then
                            "removed"

                        else
                            ""
                    ]
                    [ text <| name names r.id ]
                 , Html.td [ class "number" ] [ text <| String.fromInt <| Standings.played r ]
                 , Html.td [ class "number" ] [ text <| String.fromInt r.won ]
                 ]
                    ++ draws [ Html.td [ class "number" ] [ text <| String.fromInt r.drawn ] ]
                    ++ [ Html.td [ class "number" ] [ text <| String.fromInt r.lost ] ]
                    ++ List.map (\value -> Html.td [] [ text value ]) r.extra
                )
    in
    div [ class "leaderboard-table" ]
        [ Html.table []
            [ Html.thead []
                [ Html.tr [] <|
                    List.map (\( header, kind ) -> Html.th [ class kind ] [ text header ]) <|
                        [ ( "#", "place" ), ( "Name", "name" ), ( "Played", "number" ), ( "W", "number" ) ]
                            ++ draws [ ( "D", "number" ) ]
                            ++ [ ( "L", "number" ) ]
                            ++ List.map (\column -> ( column, "" )) standings.columns
                ]
            , Html.tbody [] <| List.indexedMap viewRow standings.rows
            ]
        ]
