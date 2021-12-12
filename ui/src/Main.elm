module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Debug.Extra
import Browser
import Bracket.KO
import Dict exposing (Dict)
import Json.Decode as JD
import Random
import Random.List

type alias Model =
    { scale: Int
    , nextId: Int
    , title: String
    , user: Dict Int String
    , userInput: String
    , hide: Bool
    , showUser: Bool
    , game: Maybe (Bracket.KO.Game Int)
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

main : Program () Model Msg
main = Browser.element
    { init = \() -> init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

init : (Model, Cmd Msg)
init = Tuple.pair
    { scale = 0
    , nextId = 0
    , title = "Tournament"
    , user = Dict.empty
    , userInput = ""
    , hide = False
    , showUser = True
    , game = Nothing
    }
    Cmd.none

view : Model -> Html Msg
view model =
    div []
        [ Html.node "link"
            [ HA.attribute "rel" "stylesheet"
            , HA.attribute "property" "stylesheet"
            , HA.attribute "href" "css/root.css"
            ] []
        , div [ class "layout" ]
            [ div [ class "title-bar" ]
                [ Html.input
                    [ class "title"
                    , HA.value model.title
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
                , Maybe.map (viewGameBox model.hide model.scale) model.game
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
                    if model.userInput /= "" && not (List.member model.userInput <| Dict.values model.user)
                    then AddName
                    else None
                , HA.placeholder "Insert username and press Enter"
                ] []
            ]
        , div [ class "user-input-list" ]
            <| List.map
                (\(id, name) ->
                    if model.game
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
            <| Dict.toList model.user
        , div [ class "user-input-start" ]
            [ Html.button
                [ HE.onClick Generate 
                , HA.disabled <| Dict.size model.user < 2
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        None -> (model, Cmd.none)
        SetScale scale -> ({model | scale = scale }, Cmd.none)
        WrapGame sub ->
            case model.game of
                Just game -> ({model | game = Just <| Bracket.KO.update sub game }, Cmd.none)
                Nothing -> (model, Cmd.none)
        SetInput input -> ({model | userInput = input }, Cmd.none)
        AddName -> Tuple.pair
            (   { model
                | userInput = ""
                , user = Dict.insert model.nextId model.userInput model.user
                , nextId = model.nextId + 1
                , game = Maybe.map
                    (\game ->
                        { game
                        | player = Dict.insert
                            model.nextId
                            { id = model.nextId
                            , name = model.userInput
                            , alive = True
                            }
                            game.player
                        }
                    )
                    model.game
                }
                |> \newModel ->
                    case newModel.game of
                        Nothing -> newModel
                        Just game -> game
                            |> Bracket.KO.addPosition
                            |> Maybe.map
                                (\new -> { newModel | game = Just new })
                            |> Maybe.withDefault newModel
            )
            Cmd.none
        Generate -> Tuple.pair model
            <| Random.generate StartGame
            <| Random.List.shuffle
            <| Dict.toList model.user
        StartGame list -> Tuple.pair
            { model
            | game = Just
                <| Bracket.KO.setInitialPlayer list
                <| Bracket.KO.generateTournament
                <| List.length list
            , showUser = False
            }
            Cmd.none
        ChangeUserName id name -> Tuple.pair
            { model
            | user = Dict.insert id name model.user
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
                model.game
            }
            Cmd.none
        RemoveUser id -> Tuple.pair
            ( case model.game of
                Nothing ->
                    { model | user = Dict.remove id model.user }
                Just game ->
                    { model
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

            )
            Cmd.none
        SetHide hide -> ({ model | hide = hide }, Cmd.none)
        SetShowUser show -> ({ model | showUser = show }, Cmd.none)
        SetTitle title -> ({ model | title = title }, Cmd.none)
