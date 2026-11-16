module Bracket.Swiss.View exposing (Model, Msg(..), Side(..), init, update, view)

{-| The rounds and the standings of a Swiss tournament. Unfinished input and
the expanded rounds stay in the `Model` of this viewer and aren't shared.
-}

import Bracket.ScoreInput as ScoreInput
import Bracket.Scoring as Scoring exposing (Cell(..), Scoring(..))
import Bracket.Swiss as Swiss exposing (Match, Round, Standing, State)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import PlayerId exposing (PlayerId)
import Set exposing (Set)


{-| A match only has a draft while its text isn't a complete result. Drafts
are keyed by the home and the away player. `expanded` has the numbers of the
earlier rounds that are shown, starting with 1.
-}
type alias Model =
    { drafts : Dict ( PlayerId, PlayerId ) ( String, String )
    , expanded : Set Int
    }


type Side
    = HomeSide
    | AwaySide


{-| The home player comes first, the away player second.
-}
type Msg
    = SetScoreText PlayerId PlayerId Side String
    | SetOutcome PlayerId PlayerId ( Int, Int )
    | Clear PlayerId PlayerId
    | NextRound
    | ToggleRound Int


init : Model
init =
    { drafts = Dict.empty
    , expanded = Set.empty
    }


currentCell : PlayerId -> PlayerId -> State -> Cell
currentCell home away state =
    Swiss.currentRound state.game
        |> Maybe.map .matches
        |> Maybe.withDefault []
        |> List.filter (\match -> match.home == home && match.away == away)
        |> List.head
        |> Maybe.map (Swiss.matchCell state.game)
        |> Maybe.withDefault Open


update : Msg -> Model -> State -> ( Model, State )
update msg model state =
    case msg of
        SetScoreText home away side value ->
            let
                ( a, b ) =
                    ScoreInput.texts (Dict.get ( home, away ) model.drafts) (currentCell home away state)

                draft =
                    case side of
                        HomeSide ->
                            ( value, b )

                        AwaySide ->
                            ( a, value )
            in
            case ScoreInput.parse draft of
                Just score ->
                    ( { model | drafts = Dict.remove ( home, away ) model.drafts }
                    , Swiss.update (Swiss.SetResult home away score) state
                    )

                Nothing ->
                    ( { model | drafts = Dict.insert ( home, away ) draft model.drafts }
                    , state
                    )

        SetOutcome home away score ->
            ( model, Swiss.update (Swiss.SetResult home away score) state )

        Clear home away ->
            ( { model | drafts = Dict.remove ( home, away ) model.drafts }
            , Swiss.update (Swiss.ClearResult home away) state
            )

        NextRound ->
            ( { model | drafts = Dict.empty }
            , Swiss.update Swiss.NextRound state
            )

        ToggleRound number ->
            ( { model
                | expanded =
                    if Set.member number model.expanded then
                        Set.remove number model.expanded

                    else
                        Set.insert number model.expanded
              }
            , state
            )


type alias Config =
    { names : Dict PlayerId String
    , canEdit : Bool
    }


view : Config -> Model -> State -> Html Msg
view config model state =
    let
        rounds =
            List.indexedMap (\index round -> ( index + 1, round )) state.game.rounds
                |> List.reverse
    in
    div [ class "swiss" ]
        [ div [ class "swiss-rounds" ] <|
            case rounds of
                ( number, current ) :: earlier ->
                    viewCurrentRound config model state number current
                        :: List.map (viewEarlierRound config model state) earlier

                [] ->
                    []
        , viewStandings config state
        ]


nameOf : Config -> PlayerId -> String
nameOf config id =
    Maybe.withDefault "" <| Dict.get id config.names


viewPlayer : Config -> State -> PlayerId -> Html msg
viewPlayer config state id =
    Html.span
        [ HA.classList
            [ ( "player", True )
            , ( "alive", not <| Swiss.isRemoved id state.game )
            ]
        ]
        [ text <| nameOf config id ]


roundTitle : State -> Int -> String
roundTitle state number =
    "Round " ++ String.fromInt number ++ " of " ++ String.fromInt state.game.totalRounds


viewCurrentRound : Config -> Model -> State -> Int -> Round -> Html Msg
viewCurrentRound config model state number round =
    Html.section [ class "swiss-round swiss-current" ]
        [ div [ class "swiss-round-title" ] [ text <| roundTitle state number ]
        , viewMatches config state (viewResult config model state) round
        , if Swiss.isFinished state.game then
            div [ class "swiss-note" ] [ text "The tournament is finished." ]

          else if config.canEdit then
            let
                ready =
                    Swiss.canStartNextRound state.game
            in
            Html.button
                [ class "swiss-next"
                , HA.disabled <| not ready
                , HA.title <|
                    if ready then
                        "Pair the players for the next round"

                    else
                        "Every match needs a result first"
                , HE.onClick NextRound
                ]
                [ text "Next round" ]

          else
            text ""
        ]


viewEarlierRound : Config -> Model -> State -> ( Int, Round ) -> Html Msg
viewEarlierRound config model state ( number, round ) =
    let
        expanded =
            Set.member number model.expanded
    in
    Html.section [ class "swiss-round swiss-earlier" ]
        [ Html.button
            [ class "swiss-round-title"
            , HA.attribute "aria-expanded" <|
                if expanded then
                    "true"

                else
                    "false"
            , HE.onClick <| ToggleRound number
            ]
            [ text <|
                (if expanded then
                    "▾ "

                 else
                    "▸ "
                )
                    ++ roundTitle state number
            ]
        , if expanded then
            viewMatches config
                state
                (\match -> [ text <| ScoreInput.readOnly state.settings.scoring <| Swiss.matchCell state.game match ])
                round

          else
            text ""
        ]


viewMatches : Config -> State -> (Match -> List (Html msg)) -> Round -> Html msg
viewMatches config state viewMatchResult round =
    Html.table [ class "swiss-matches" ] <|
        List.map
            (\match ->
                let
                    c =
                        Swiss.matchCell state.game match
                in
                Html.tr []
                    [ Html.td
                        [ HA.classList
                            [ ( "swiss-home", True )
                            , ( ScoreInput.outcomeClass c, True )
                            , ( "removed", Swiss.isRemoved match.home state.game )
                            ]
                        ]
                        [ viewPlayer config state match.home ]
                    , Html.td [ class "swiss-result score-host" ] <| viewMatchResult match
                    , Html.td
                        [ HA.classList
                            [ ( "swiss-away", True )
                            , ( ScoreInput.outcomeClass <| Scoring.flip c, True )
                            , ( "removed", Swiss.isRemoved match.away state.game )
                            ]
                        ]
                        [ viewPlayer config state match.away ]
                    ]
            )
            round.matches
            ++ (case round.bye of
                    Just id ->
                        [ Html.tr [ class "swiss-bye" ]
                            [ Html.td
                                [ HA.classList
                                    [ ( "swiss-home", True )
                                    , ( "win", True )
                                    , ( "removed", Swiss.isRemoved id state.game )
                                    ]
                                ]
                                [ viewPlayer config state id ]
                            , Html.td [ class "swiss-result", HA.title "A bye counts as a win" ] [ text "bye" ]
                            , Html.td [] []
                            ]
                        ]

                    Nothing ->
                        []
               )


viewResult : Config -> Model -> State -> Match -> List (Html Msg)
viewResult config model state match =
    let
        c =
            Swiss.matchCell state.game match

        draft =
            Dict.get ( match.home, match.away ) model.drafts
    in
    if config.canEdit && Swiss.isEditable match.home match.away state.game then
        ScoreInput.view
            { scoring = state.settings.scoring
            , cell = c
            , texts = ScoreInput.texts draft c
            , draft = draft /= Nothing
            , names = ( nameOf config match.home, nameOf config match.away )
            , onFirst = SetScoreText match.home match.away HomeSide
            , onSecond = SetScoreText match.home match.away AwaySide
            , onOutcome = SetOutcome match.home match.away
            , onClear = Clear match.home match.away
            }

    else
        [ text <| ScoreInput.readOnly state.settings.scoring c ]


viewStandings : Config -> State -> Html msg
viewStandings config state =
    let
        number f =
            f >> String.fromInt >> text

        columns : List { title : String, description : String, class : String, cell : Standing -> Html msg }
        columns =
            [ { title = "Place"
              , description = "The current place"
              , class = "swiss-stat"
              , cell = .place >> Maybe.map String.fromInt >> Maybe.withDefault "–" >> text
              }
            , { title = "Name"
              , description = ""
              , class = "swiss-name"
              , cell = .id >> viewPlayer config state
              }
            , { title = "W-D-L"
              , description = "Wins, draws and losses. A bye counts as a win."
              , class = "swiss-stat"
              , cell = \s -> text <| String.join "-" <| List.map String.fromInt [ s.wins, s.draws, s.losses ]
              }
            ]
                ++ (if state.settings.scoring == Points then
                        [ { title = "Score"
                          , description = "Points scored and conceded"
                          , class = "swiss-stat"
                          , cell = \s -> text <| String.fromInt s.scored ++ " : " ++ String.fromInt s.conceded
                          }
                        ]

                    else
                        []
                   )
                ++ [ { title = "Pts"
                     , description = "Match points: 3 for a win, 1 for a draw"
                     , class = "swiss-stat"
                     , cell = number .matchPoints
                     }
                   , { title = "Buchholz"
                     , description = "The sum of the match points of all opponents"
                     , class = "swiss-stat"
                     , cell = number .buchholz
                     }
                   ]
    in
    Html.table [ class "swiss-standings" ]
        [ Html.thead []
            [ Html.tr [] <|
                List.map
                    (\column ->
                        Html.th [ class column.class, HA.title column.description ] [ text column.title ]
                    )
                    columns
            ]
        , Html.tbody [] <|
            List.map
                (\standing ->
                    Html.tr [ HA.classList [ ( "removed", standing.place == Nothing ) ] ] <|
                        List.map (\column -> Html.td [ class column.class ] [ column.cell standing ]) columns
                )
                (Swiss.standings state)
        ]
