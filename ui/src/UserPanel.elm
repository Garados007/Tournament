module UserPanel exposing (Model, Msg(..), init, update, view, viewToggler)

import Data exposing (Data)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Json.Decode as JD
import ModeSelector
import PlayerId exposing (PlayerId)
import Random
import Random.List
import Tournament


type alias Model =
    { input : String
    , open : Bool
    , modeSelector : ModeSelector.Model
    }


type Msg
    = SetInput String
    | AddName
    | Generate
    | StartGame (List PlayerId)
    | ChangeUserName PlayerId String
    | RemoveUser PlayerId
    | RestoreUser PlayerId
    | MovePlayer PlayerId Int
    | SetOpen Bool
    | SettingsMsg ModeSelector.Msg


init : Bool -> Model
init open =
    { input = ""
    , open = open
    , modeSelector = ModeSelector.init
    }


{-| A tournament can stop taking new players, then the name field is disabled
until the next start.
-}
acceptsPlayers : Data -> Bool
acceptsPlayers data =
    Maybe.map Tournament.acceptsPlayers data.tournament
        |> Maybe.withDefault True


canAdd : Data -> String -> Bool
canAdd data name =
    acceptsPlayers data && name /= "" && not (List.member name <| Dict.values data.users)


update : Msg -> Model -> Data -> ( Model, Data, Cmd Msg )
update msg model data =
    case msg of
        SetInput input ->
            ( { model | input = input }, data, Cmd.none )

        AddName ->
            if canAdd data model.input then
                ( { model | input = "" }
                , Data.addPlayer model.input data
                , Cmd.none
                )

            else
                ( model, data, Cmd.none )

        Generate ->
            let
                remaining =
                    Data.filterPlayers (canStart data) data
            in
            case data.nextSettings.order of
                Tournament.Shuffled ->
                    ( model
                    , remaining
                    , Random.generate StartGame <|
                        Random.List.shuffle remaining.order
                    )

                Tournament.Seeded ->
                    startGame remaining.order model remaining

        StartGame list ->
            startGame list model data

        ChangeUserName id name ->
            ( model
            , { data | users = Dict.insert id name data.users }
            , Cmd.none
            )

        RemoveUser id ->
            ( model
            , case data.tournament of
                Nothing ->
                    Data.filterPlayers ((/=) id) data

                Just tournament ->
                    { data | tournament = Just <| Tournament.removePlayer id tournament }
            , Cmd.none
            )

        RestoreUser id ->
            ( model
            , case Maybe.andThen (Tournament.restorePlayer id) data.tournament of
                Just tournament ->
                    { data | tournament = Just tournament }

                Nothing ->
                    data
            , Cmd.none
            )

        MovePlayer id step ->
            ( model, Data.movePlayer (canStart data) id step data, Cmd.none )

        SetOpen open ->
            ( { model | open = open }, data, Cmd.none )

        SettingsMsg sub ->
            let
                ( modeSelector, settings ) =
                    ModeSelector.update sub model.modeSelector data.nextSettings
            in
            ( { model | modeSelector = modeSelector }
            , { data | nextSettings = settings }
            , Cmd.none
            )


canStart : Data -> PlayerId -> Bool
canStart data =
    Tournament.takesPart data.nextSettings.entrants data.tournament


{-| Whether the player is out of the running tournament without being removed.
-}
isOut : Data -> PlayerId -> Bool
isOut data id =
    case data.tournament of
        Just tournament ->
            Tournament.isEliminated id tournament && not (Tournament.isRemoved id tournament)

        Nothing ->
            False


isRestorable : Data -> PlayerId -> Bool
isRestorable data id =
    Maybe.andThen (Tournament.restorePlayer id) data.tournament /= Nothing


startGame : List PlayerId -> Model -> Data -> ( Model, Data, Cmd Msg )
startGame list model data =
    ( { model | open = False }
    , { data | tournament = Just <| Tournament.start data.nextSettings list }
    , Cmd.none
    )


viewToggler : Model -> Html Msg
viewToggler model =
    div
        [ HA.classList
            [ ( "user-toggler", True )
            , ( "open", model.open )
            ]
        , HE.onClick <| SetOpen <| not model.open
        ]
    <|
        List.repeat 3 <|
            div [] []


onEnter : msg -> Html.Attribute msg
onEnter event =
    HE.custom "keydown" <|
        JD.andThen
            (\code ->
                if code == 13 then
                    JD.succeed
                        { message = event
                        , stopPropagation = True
                        , preventDefault = True
                        }

                else
                    JD.fail "enter"
            )
        <|
            HE.keyCode


view : Bool -> Data -> Model -> Html Msg
view canEdit data model =
    div
        [ HA.classList
            [ ( "layout-user", True )
            , ( "open", model.open )
            ]
        ]
        [ div [ class "user-input" ]
            [ if canEdit then
                if acceptsPlayers data then
                    div [ class "user-input-box" ]
                        [ Html.input
                            [ HA.type_ "text"
                            , HA.value model.input
                            , HE.onInput SetInput
                            , onEnter AddName
                            , HA.placeholder "Enter a name and press Enter"
                            ]
                            []
                        ]

                else
                    div [ class "user-input-box" ]
                        [ Html.input
                            [ HA.type_ "text"
                            , HA.disabled True
                            , HA.placeholder "Players can't join after the first result"
                            ]
                            []
                        ]

              else
                text ""
            , div [ class "user-input-list" ] <|
                List.map (viewEntry canEdit data) <|
                    entries data
            , if canEdit then
                Html.map SettingsMsg <| ModeSelector.view model.modeSelector data.nextSettings

              else
                text ""
            , if canEdit then
                div [ class "user-input-start" ]
                    [ Html.button
                        [ HE.onClick Generate
                        , HA.disabled <|
                            Dict.size data.users
                                < Tournament.minPlayers data.nextSettings.mode
                        ]
                        [ text "Start new tournament" ]
                    ]

              else
                text ""
            ]
        ]


{-| A player in the list. Only players who take part in the next start have a
seed. `out` players are eliminated from the running tournament.
-}
type alias Entry =
    { id : PlayerId
    , name : String
    , seed : Maybe Int
    , out : Bool
    }


{-| The players in their order.
-}
entries : Data -> List Entry
entries data =
    data.order
        |> List.foldl
            (\id ( list, next ) ->
                case Dict.get id data.users of
                    Just name ->
                        if canStart data id then
                            ( { id = id, name = name, seed = Just next, out = isOut data id } :: list, next + 1 )

                        else
                            ( { id = id, name = name, seed = Nothing, out = isOut data id } :: list, next )

                    Nothing ->
                        ( list, next )
            )
            ( [], 1 )
        |> Tuple.first
        |> List.reverse


{-| The seeds are only shown while the seeded order is selected.
-}
viewEntry : Bool -> Data -> Entry -> Html Msg
viewEntry canEdit data entry =
    case entry.seed of
        Nothing ->
            div [ class "user-input-entry deleted" ]
                [ Html.span [ class "user-name" ] [ text entry.name ]
                , viewOut entry
                , if canEdit && isRestorable data entry.id then
                    Html.button
                        [ class "user-restore"
                        , HA.title "Undo the removal"
                        , HE.onClick <| RestoreUser entry.id
                        ]
                        [ text "↶" ]

                  else
                    text ""
                ]

        Just seed ->
            div [ class "user-input-entry" ] <|
                List.concat
                    [ if data.nextSettings.order == Tournament.Seeded then
                        [ div [ class "user-seed" ] [ text <| String.fromInt seed ] ]

                      else
                        []
                    , [ Html.input
                            [ HA.value entry.name
                            , HE.onInput <| ChangeUserName entry.id
                            ]
                            []
                      , viewOut entry
                      ]
                    , if canEdit then
                        [ Html.button
                            [ class "user-move"
                            , HA.title "Move up"
                            , HE.onClick <| MovePlayer entry.id -1
                            ]
                            [ text "▲" ]
                        , Html.button
                            [ class "user-move"
                            , HA.title "Move down"
                            , HE.onClick <| MovePlayer entry.id 1
                            ]
                            [ text "▼" ]
                        ]

                      else
                        []
                    , [ Html.button
                            [ HE.onClick <| RemoveUser entry.id ]
                            [ text "❌" ]
                      ]
                    ]


viewOut : Entry -> Html msg
viewOut entry =
    if entry.out then
        Html.span
            [ class "user-out"
            , HA.title "Eliminated from the running tournament"
            ]
            [ text "out" ]

    else
        text ""
