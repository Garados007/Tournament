module Bracket.RoundRobin.View exposing (Model, Msg(..), Side(..), init, update, view)

{-| The table of a round robin. Unfinished input stays in the `Model` of this
viewer and isn't shared.
-}

import Bracket.RoundRobin as RR exposing (Cell(..), Matches(..), Outcome(..), Ranking(..), Scoring(..), Standing, State)
import Dict exposing (Dict)
import GameBox exposing (Headers(..))
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import PlayerId exposing (PlayerId)


{-| The raw text of the score inputs, keyed by row and column. A cell only has
a draft while its text isn't a complete result.
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


scoreTexts : PlayerId -> PlayerId -> Model -> State -> ( String, String )
scoreTexts row column model state =
    case Dict.get ( row, column ) model.drafts of
        Just draft ->
            draft

        Nothing ->
            case RR.cell row column state of
                Played ( a, b ) ->
                    ( String.fromInt a, String.fromInt b )

                _ ->
                    ( "", "" )


{-| The cells of a single match show the same result, so both lose their
drafts.
-}
dropDrafts : PlayerId -> PlayerId -> State -> Model -> Model
dropDrafts row column state model =
    { model
        | drafts =
            model.drafts
                |> Dict.remove ( row, column )
                |> (if state.settings.matches == Single then
                        Dict.remove ( column, row )

                    else
                        identity
                   )
    }


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

                parse =
                    String.trim >> String.toInt
            in
            case Tuple.mapBoth parse parse draft of
                ( Just scoreA, Just scoreB ) ->
                    ( dropDrafts row column state model
                    , RR.update (RR.SetResult row column ( scoreA, scoreB )) state
                    )

                _ ->
                    ( { model | drafts = Dict.insert ( row, column ) draft model.drafts }
                    , state
                    )

        SetOutcome row column score ->
            ( model, RR.update (RR.SetResult row column score) state )

        Clear row column ->
            ( dropDrafts row column state model
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
                                                        , ( outcomeClass <| RR.cell row.id column.id state, True )
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


outcomeClass : Cell -> String
outcomeClass c =
    case ( c, RR.cellOutcome c ) of
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


viewCell : Config -> Model -> State -> ( PlayerId, String ) -> ( PlayerId, String ) -> List (Html Msg)
viewCell config model state ( row, rowName ) ( column, columnName ) =
    let
        c =
            RR.cell row column state

        editable =
            config.canEdit && RR.isEditable row column state.game

        hasDraft =
            Dict.member ( row, column ) model.drafts

        clear =
            if editable && (hasDraft || c /= Open) then
                Html.button
                    [ class "rr-clear"
                    , HA.title "Clear the result"
                    , HE.onClick <| Clear row column
                    ]
                    []

            else
                text ""
    in
    if not editable then
        [ text <|
            case ( c, state.settings.scoring ) of
                ( Played ( a, b ), Points ) ->
                    String.fromInt a ++ " : " ++ String.fromInt b

                ( Played _, WinnerOnly ) ->
                    outcomeLetter c

                ( Forfeit _, _ ) ->
                    "w/o"

                ( Cancelled, _ ) ->
                    "–"

                ( Open, _ ) ->
                    ""
        ]

    else
        case state.settings.scoring of
            Points ->
                let
                    ( a, b ) =
                        scoreTexts row column model state

                    input side value title =
                        Html.input
                            [ class "rr-score"
                            , HA.value value
                            , HA.title title
                            , HA.attribute "aria-label" title
                            , HA.attribute "autocomplete" "off"
                            , HE.onInput <| SetScoreText row column side
                            ]
                            []
                in
                [ div [ HA.classList [ ( "rr-scores", True ), ( "draft", hasDraft ) ] ]
                    [ input RowSide a <| "Score of " ++ rowName
                    , text ":"
                    , input ColumnSide b <| "Score of " ++ columnName
                    ]
                , clear
                ]

            WinnerOnly ->
                [ div [ class "rr-outcome" ] <|
                    List.map
                        (\( score, letter, title ) ->
                            Html.button
                                [ HA.classList [ ( "selected", c == Played score ) ]
                                , HA.attribute "aria-pressed" <|
                                    if c == Played score then
                                        "true"

                                    else
                                        "false"
                                , HA.title title
                                , HE.onClick <| SetOutcome row column score
                                ]
                                [ text letter ]
                        )
                        [ ( ( 1, 0 ), "W", rowName ++ " wins" )
                        , ( ( 0, 0 ), "D", "Draw" )
                        , ( ( 0, 1 ), "L", columnName ++ " wins" )
                        ]
                , clear
                ]


outcomeLetter : Cell -> String
outcomeLetter c =
    case RR.cellOutcome c of
        Just Win ->
            "W"

        Just Draw ->
            "D"

        Just Loss ->
            "L"

        Nothing ->
            ""
