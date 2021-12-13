module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Debug.Extra
import Browser
import Bracket.KO
import Data exposing (Data)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Random
import Random.List
import Process
import Task

type alias Model =
    { scale: Int
    , userInput: String
    , hide: Bool
    , showUser: Bool
    , data: Data
    , changeset: Int
    }

type Msg
    = None
    | SetScale Int
    | WrapGame Bracket.KO.Msg
    | SetInput String
    | AddName
    | Generate
    | StartGame (List (Int, String))
    | ChangeUserName Int String
    | RemoveUser Int
    | SetHide Bool
    | SetShowUser Bool
    | SetTitle String
    | UploadData Int

main : Program () Model Msg
main = Browser.document
    { init = \() -> init
    , view = \model ->
        Browser.Document model.data.title
            <| view model
    , update = update
    , subscriptions = always Sub.none
    }

init : (Model, Cmd Msg)
init = Tuple.pair
    { scale = 0
    , userInput = ""
    , hide = False
    , showUser = True
    , data = Data.init
    , changeset = 0
    }
    Cmd.none

view : Model -> List (Html Msg)
view model =
    [ Html.node "link"
        [ HA.attribute "rel" "stylesheet"
        , HA.attribute "property" "stylesheet"
        , HA.attribute "href" "css/root.css"
        ] []
    , div [ class "layout" ]
        [ div [ class "title-bar" ]
            [ Html.input
                [ class "title"
                , HA.value model.data.title
                , HE.onInput SetTitle
                ] []
            , div
                [ HA.classList
                    [ ("user-toggler", True)
                    , ("open", model.showUser)
                    ]
                , HE.onClick <| SetShowUser <| not model.showUser
                ]
                <| List.repeat 3
                <| div [] []
            ]
        , div [ class "layout-content" ]
            [ div 
                [ HA.classList
                    [ ("layout-user", True)
                    , ("open", model.showUser)
                    ]
                ]
                [ viewUserInput model ]
            , Maybe.map (viewGameBox model.hide model.scale) model.data.game
                |> Maybe.withDefault
                    ( div [ class "layout-replacement" ]
                        [ text "There is currently no tournament existing.\nTry to create one." ]
                    )
            ]
        ]
    -- , Debug.Extra.viewModel <| Maybe.map Bracket.KO.hideFinishedPhases model.game
    ]

onEnter : msg -> Html.Attribute msg
onEnter event =
    HE.custom "keydown"
        <| JD.andThen
            (\code ->
                if code == 13
                then JD.succeed
                    { message = event
                    , stopPropagation = True
                    , preventDefault = True
                    }
                else JD.fail "enter"
            )
        <| HE.keyCode

viewUserInput : Model -> Html Msg
viewUserInput model =
    div [ class "user-input" ]
        [ div [ class "user-input-box" ]
            [ Html.input
                [ HA.type_ "text"
                , HA.value model.userInput
                , HE.onInput SetInput
                , onEnter <|
                    if model.userInput /= ""
                        && not 
                            (List.member model.userInput
                                <| Dict.values model.data.user
                            )
                    then AddName
                    else None
                , HA.placeholder "Insert username and press Enter"
                ] []
            ]
        , div [ class "user-input-list" ]
            <| List.map
                (\(id, name) ->
                    if model.data.game
                        |> Maybe.andThen (.player >> Dict.get id)
                        |> Maybe.map (.alive >> not)
                        |> Maybe.withDefault False
                    then div [ class "user-input-entry deleted" ] 
                        [ text name ]
                    else div [ class "user-input-entry" ]
                        [ Html.input
                            [ HA.value name
                            , HE.onInput <| ChangeUserName id
                            ]
                            []
                        , Html.button
                            [ HE.onClick <| RemoveUser id ]
                            [ text "❌" ]
                        ]
                )
            <| Dict.toList model.data.user
        , div [ class "user-input-start" ]
            [ Html.button
                [ HE.onClick Generate 
                , HA.disabled <| Dict.size model.data.user < 2
                ]
                [ text "Start new Tournament" ]
            ]
        ]

viewGameBox : Bool -> Int -> Bracket.KO.Game Int -> Html Msg
viewGameBox hide scale game =
    div [ class "game-box" ]
        [ div [ class "game-box-controls" ]
            [ Html.button
                [ HE.onClick <| SetHide <| not hide 
                , HA.title <|
                    if hide
                    then "Show all phases in the tournament again"
                    else "Hide finished phases"
                ]
                [ text <| if hide then "🔶" else "🔸" ]
            , Html.button
                [ HE.onClick <| SetScale <| scale + 1
                , HA.title "Zoom out"
                ]
                [ text "-" ]
            , Html.button
                [ HE.onClick <| SetScale <| scale - 1
                , HA.title "Zoom in"
                ]
                [ text "+" ]
            ]
        , div 
            [ class "game-box-scaler"
            , HA.style "font-size"
                <| String.fromFloat (0.75 ^ toFloat scale) ++ "em"
            ]
            [ Html.map WrapGame 
                <| Bracket.KO.viewGame
                <| if hide then Bracket.KO.hideFinishedPhases game else game
            ]
        ]

updateData : Model -> (Data -> Data) -> (Model, Cmd Msg)
updateData model updater =
    let
        newData = updater model.data

        cs = model.changeset + 1
    in if newData == model.data
        then (model, Cmd.none)
        else Tuple.pair
            { model | data = newData, changeset = cs }
            <| Task.perform (always <| UploadData cs)
            <| Process.sleep 100

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        None -> (model, Cmd.none)
        SetScale scale -> ({model | scale = scale }, Cmd.none)
        WrapGame sub ->
            case model.data.game of
                Just game -> updateData model
                    <| \data ->
                        { data
                        | game = Just <| Bracket.KO.update sub game
                        }
                Nothing -> (model, Cmd.none)
        SetInput input -> ({model | userInput = input }, Cmd.none)
        AddName ->
            updateData { model | userInput = "" }
            <|  (\data ->
                    case data.game of
                        Nothing -> data
                        Just game -> game
                            |> Bracket.KO.addPosition
                            |> Maybe.map
                                (\new -> { data | game = Just new })
                            |> Maybe.withDefault data
                )
            << \data ->
                { data
                | user = Dict.insert data.nextId model.userInput data.user
                , nextId = data.nextId + 1
                , game = Maybe.map
                    (\game ->
                        { game
                        | player = Dict.insert
                            data.nextId
                            { id = data.nextId
                            , name = model.userInput
                            , alive = True
                            }
                            game.player
                        }
                    )
                    data.game
                }
        Generate ->
            updateData model
                (\data ->
                    { data
                    | user = case data.game of
                        Nothing -> data.user
                        Just game -> Dict.filter
                            (\id _ ->
                                Dict.get id game.player
                                |> Maybe.map .alive
                                |> Maybe.withDefault True
                            )
                            data.user
                    }
                )
            |> Tuple.mapSecond
                (\cmd ->
                    Cmd.batch
                        [ cmd
                        , Random.generate StartGame
                            <| Random.List.shuffle
                            <|  ( case model.data.game of
                                    Nothing -> identity
                                    Just game ->
                                        List.filter
                                            <| \(id, _) ->
                                                Dict.get id game.player
                                                |> Maybe.map .alive
                                                |> Maybe.withDefault True
                                )
                            <| Dict.toList model.data.user
                        ]
                )
        StartGame list -> 
            updateData { model | showUser = False }
            <| \data ->
                { data
                | game = Just
                    <| Bracket.KO.setInitialPlayer list
                    <| Bracket.KO.generateTournament
                    <| List.length list
                }
        ChangeUserName id name ->
            updateData model
            <| \data ->
                { data
                | user = Dict.insert id name data.user
                , game = Maybe.map
                    (\game ->
                        { game
                        | player = Dict.update id
                            (Maybe.map
                                <| \player ->
                                    { player | name = name }
                            )
                            game.player
                        }
                    )
                    data.game
                }
        RemoveUser id ->
            updateData model
            <| \data -> case data.game of
                Nothing ->
                    { data | user = Dict.remove id data.user }
                Just game ->
                    { data
                    | game = Just
                        { game
                        | player = Dict.update id
                            (Maybe.map
                                <| \player -> { player | alive = False }
                            )
                            game.player
                        , slots = Dict.values game.levels
                            |> List.concatMap
                                ( Dict.values
                                    >> List.filterMap
                                        (\phase -> phase.winner
                                            |> Maybe.andThen
                                                (\winner ->
                                                    ( if Dict.get winner game.slots == Nothing
                                                        then
                                                            [ (phase.player1, phase.player2)
                                                            , (phase.player2, phase.player1)
                                                            ]
                                                        else []
                                                    )
                                                    |> List.map
                                                        (Tuple.mapFirst
                                                            <| \x -> Dict.get x game.slots
                                                        )
                                                    |> List.filter
                                                        (Tuple.first >> (==) (Just id))
                                                    |> List.map Tuple.second
                                                    |> List.head
                                                    |> Maybe.andThen
                                                        (\x -> Dict.get x game.slots)
                                                    |> Maybe.map (Tuple.pair winner)
                                                )

                                        )
                                )
                            |> List.foldl
                                (\(winnerSlot, winnerId) -> Dict.insert winnerSlot winnerId)
                                game.slots
                        }
                    }
        SetHide hide -> ({ model | hide = hide }, Cmd.none)
        SetShowUser show -> ({ model | showUser = show }, Cmd.none)
        SetTitle title -> updateData model <| \data -> { data | title = title }
        UploadData changeset ->
            if model.changeset == changeset
            then
                let
                    d_ = Debug.log "data" model.data
                in (model, Cmd.none)
            else (model, Cmd.none)
