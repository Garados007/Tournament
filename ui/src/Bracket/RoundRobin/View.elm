module Bracket.RoundRobin.View exposing (Model, Msg(..), Side(..), init, update, view)

{-| The table of a round robin. Unfinished input stays in the `Model` of this
viewer and isn't shared.
-}

import Bracket.RoundRobin as RR exposing (Ranking(..), Standing, State)
import Bracket.ScoreInput as ScoreInput
import Bracket.Scoring exposing (Scoring(..))
import Dict exposing (Dict)
import GameBox exposing (Headers(..))
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import PlayerId exposing (PlayerId)


{-| The raw text of the score inputs. A match only has a draft while its text
isn't a complete result. Drafts are keyed and ordered like the results, so both
cells of a single match share one draft.
-}
type alias Model =
    { drafts : Dict ( PlayerId, PlayerId ) ( String, String )
    }


type Side
    = RowSide
    | ColumnSide


type Msg
    = SetScoreText PlayerId PlayerId Side String
    | SetOutcome PlayerId PlayerId ( Int, Int )
    | Clear PlayerId PlayerId


init : Model
init =
    { drafts = Dict.empty }


swapTexts : ( String, String ) -> ( String, String )
swapTexts ( a, b ) =
    ( b, a )


getDraft : PlayerId -> PlayerId -> Model -> State -> Maybe ( String, String )
getDraft row column model state =
    let
        ( key, swapped ) =
            RR.resultKey state.settings.matches row column
    in
    Dict.get key model.drafts
        |> Maybe.map
            (if swapped then
                swapTexts

             else
                identity
            )


setDraft : PlayerId -> PlayerId -> ( String, String ) -> State -> Model -> Model
setDraft row column draft state model =
    let
        ( key, swapped ) =
            RR.resultKey state.settings.matches row column
    in
    { model
        | drafts =
            Dict.insert key
                (if swapped then
                    swapTexts draft

                 else
                    draft
                )
                model.drafts
    }


dropDraft : PlayerId -> PlayerId -> State -> Model -> Model
dropDraft row column state model =
    { model
        | drafts =
            Dict.remove
                (Tuple.first <| RR.resultKey state.settings.matches row column)
                model.drafts
    }


scoreTexts : PlayerId -> PlayerId -> Model -> State -> ( String, String )
scoreTexts row column model state =
    ScoreInput.texts (getDraft row column model state) (RR.cell row column state)


update : Msg -> Model -> State -> ( Model, State )
update msg model state =
    case msg of
        SetScoreText row column side value ->
            let
                ( a, b ) =
                    scoreTexts row column model state

                draft =
                    case side of
                        RowSide ->
                            ( value, b )

                        ColumnSide ->
                            ( a, value )
            in
            case ScoreInput.parse draft of
                Just score ->
                    ( dropDraft row column state model
                    , RR.update (RR.SetResult row column score) state
                    )

                Nothing ->
                    ( setDraft row column draft state model
                    , state
                    )

        SetOutcome row column score ->
            ( model, RR.update (RR.SetResult row column score) state )

        Clear row column ->
            ( dropDraft row column state model
            , RR.update (RR.ClearResult row column) state
            )


type alias Config =
    { names : Dict PlayerId String
    , canEdit : Bool
    , headers : Headers
    }


view : Config -> Model -> State -> Html Msg
view config model state =
    let
        players =
            state.game.players

        numbered =
            List.indexedMap (\index player -> ( index + 1, player )) players

        standings : Dict PlayerId Standing
        standings =
            RR.standings state
                |> List.map (\standing -> ( standing.id, standing ))
                |> Dict.fromList

        withPoints =
            state.settings.scoring == Points

        withMatchPoints =
            RR.effectiveRanking state.settings == ByResults

        name id =
            Maybe.withDefault "" <| Dict.get id config.names

        label : Bool -> Int -> PlayerId -> String
        label isColumn number id =
            case ( config.headers, isColumn ) of
                ( Names, _ ) ->
                    name id

                ( Numbers, True ) ->
                    String.fromInt number

                _ ->
                    String.fromInt number ++ ". " ++ name id

        removedClass player =
            ( "removed", player.removed )

        statHeaders =
            List.filterMap identity
                [ Just ( "W-D-L", "Wins, draws and losses" )
                , if withPoints then
                    Just ( "Score", "Points scored and conceded" )

                  else
                    Nothing
                , if withMatchPoints then
                    Just ( "Pts", "Match points: 3 for a win, 1 for a draw" )

                  else
                    Nothing
                , Just ( "Place", "The current place" )
                ]

        statCells : Standing -> List (Html Msg)
        statCells standing =
            List.filterMap identity
                [ Just <|
                    String.join "-" <|
                        List.map String.fromInt [ standing.wins, standing.draws, standing.losses ]
                , if withPoints then
                    Just <| String.fromInt standing.scored ++ " : " ++ String.fromInt standing.conceded

                  else
                    Nothing
                , if withMatchPoints then
                    Just <| String.fromInt standing.matchPoints

                  else
                    Nothing
                , Just <| Maybe.withDefault "–" <| Maybe.map String.fromInt standing.place
                ]
                |> List.map (\value -> Html.td [ class "rr-stat" ] [ text value ])
    in
    div [ class "rr" ]
        [ Html.table [ class "rr-table" ]
            [ Html.thead []
                [ Html.tr [] <|
                    Html.th [ class "rr-corner" ] []
                        :: List.map
                            (\( number, player ) ->
                                Html.th
                                    [ HA.classList [ ( "rr-column", True ), removedClass player ]
                                    , HA.title <| name player.id
                                    ]
                                    [ text <| label True number player.id ]
                            )
                            numbered
                        ++ List.map
                            (\( title, description ) ->
                                Html.th [ class "rr-stat", HA.title description ] [ text title ]
                            )
                            statHeaders
                ]
            , Html.tbody [] <|
                List.map
                    (\( number, row ) ->
                        Html.tr [ HA.classList [ removedClass row ] ] <|
                            Html.th [ class "rr-name" ]
                                [ Html.span
                                    [ HA.classList [ ( "player", True ), ( "alive", not row.removed ) ] ]
                                    [ text <| label False number row.id ]
                                ]
                                :: List.map
                                    (\column ->
                                        if row.id == column.id then
                                            Html.td [ class "rr-cell rr-self" ] []

                                        else
                                            viewCell config model state ( row.id, name row.id ) ( column.id, name column.id )
                                                |> Html.td
                                                    [ HA.classList
                                                        [ ( "rr-cell", True )
                                                        , removedClass column
                                                        , ( "score-host", True )
                                                        , ( ScoreInput.outcomeClass <| RR.cell row.id column.id state, True )
                                                        ]
                                                    ]
                                    )
                                    players
                                ++ (Dict.get row.id standings
                                        |> Maybe.map statCells
                                        |> Maybe.withDefault []
                                   )
                    )
                    numbered
            ]
        ]


viewCell : Config -> Model -> State -> ( PlayerId, String ) -> ( PlayerId, String ) -> List (Html Msg)
viewCell config model state ( row, rowName ) ( column, columnName ) =
    let
        c =
            RR.cell row column state

        draft =
            getDraft row column model state
    in
    if config.canEdit && RR.isEditable row column state.game then
        ScoreInput.view
            { scoring = state.settings.scoring
            , cell = c
            , texts = ScoreInput.texts draft c
            , draft = draft /= Nothing
            , names = ( rowName, columnName )
            , onFirst = SetScoreText row column RowSide
            , onSecond = SetScoreText row column ColumnSide
            , onOutcome = SetOutcome row column
            , onClear = Clear row column
            }

    else
        [ text <| ScoreInput.readOnly state.settings.scoring c ]
