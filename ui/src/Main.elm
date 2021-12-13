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
import Random.String
import Random.Char
import Process
import Task
import Http
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Network
import Url.Parser
import Url.Parser.Query
import Ports
import LocalStorage
import Html.Attributes exposing (value)

type alias Model =
    { scale: Int
    , userInput: String
    , hide: Bool
    , showUser: Bool
    , data: Data
    , changeset: Int
    , offline: Bool
    , creds: Maybe (String, Maybe String)
    , key: Key
    , origin: String
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
    | GotBroadcastInfo (Result Http.Error (String, String))
    | SetOrigin String
    | ReceiveData (Result String Data)
    | GetLocalStorage LocalStorage.Response
    | SocketClose (Result JD.Error Network.SocketClose)

main : Program () Model Msg
main = Browser.application
    { init = \() -> init
    , view = \model ->
        Browser.Document model.data.title
            <| view model
    , update = update
    , subscriptions = \model ->
        Sub.batch
            [ Network.wsReceive ReceiveData
            , Network.wsClose SocketClose
            , LocalStorage.responseHandler GetLocalStorage storage
                |> Ports.settingResponse
            ]
    , onUrlRequest = always None
    , onUrlChange = always None
    }

storage : LocalStorage.LocalStorage msg
storage = LocalStorage.make
    Ports.settingGetItem
    Ports.settingSetItem
    Ports.settingClear
    Ports.settingListKeys
    "tournament"

init : Url -> Key -> (Model, Cmd Msg)
init url key =
    let
        id : Maybe String
        id = { url | path = "" }
            |> Url.Parser.parse
                (Url.Parser.query
                    <| Url.Parser.Query.string "id"
                )
            |> Debug.log "id"
            |> Maybe.andThen identity

        model : Model
        model =
            { scale = 0
            , userInput = ""
            , hide = False
            , showUser = id == Nothing
            , data = Data.init
            , changeset = 0
            , offline = False
            , creds = Maybe.map (\x -> (x, Nothing)) id
            , key = key
            , origin = ""
            }
        
        cmds : List (Cmd Msg)
        cmds = 
            [ case id of
                Just id_ -> Network.wsConnect id_
                Nothing -> Http.get
                    { url = "https://relay.2complex.de/api/new"
                    , expect = Http.expectJson GotBroadcastInfo
                        <| JD.map2 Tuple.pair
                            (JD.field "id" JD.string)
                            (JD.field "token" JD.string)
                    }
            , Random.generate SetOrigin
                <| Random.String.string 40
                <| Random.Char.latin
            , Maybe.map
                (\id_ -> LocalStorage.getItem storage <| id_ ++ ".token")
                id
                |> Maybe.withDefault Cmd.none
            , Maybe.map
                (\id_ -> LocalStorage.getItem storage <| id_ ++ ".data")
                id
                |> Maybe.withDefault Cmd.none
            ]
    in (model, Cmd.batch cmds)

canEdit : Model -> Bool
canEdit model =
    model.offline ||
    ( case model.creds of
        Just (_, Nothing) -> False
        _ -> True
    )

silence : Model -> Html Msg -> Html Msg
silence model node =
    if canEdit model
    then node
    else Html.map (always None) node

view : Model -> List (Html Msg)
view model =
    [ Html.node "link"
        [ HA.attribute "rel" "stylesheet"
        , HA.attribute "property" "stylesheet"
        , HA.attribute "href" "css/root.css"
        ] []
    , div [ class "layout" ]
        [ div [ class "title-bar" ]
            [ if canEdit model
                then Html.input
                    [ class "title"
                    , HA.value model.data.title
                    , HE.onInput SetTitle
                    ] []
                else div
                    [ class "title" ]
                    [ text model.data.title ]
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
                [ silence model <| viewUserInput model ]
            , Maybe.map (viewGameBox model model.hide model.scale) model.data.game
                |> Maybe.withDefault
                    ( div [ class "layout-replacement" ]
                        <| List.singleton
                        <| text
                        <| if canEdit model
                            then "There is currently no tournament existing.\nTry to create one."
                            else "There is currently no tournament existing.\nWait for one to be created."
                    )
            , footer
            ]
        ]
    -- , Debug.Extra.viewModel <| model
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
        [ if canEdit model 
            then div [ class "user-input-box" ]
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
            else text ""
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
        , if canEdit model
            then div [ class "user-input-start" ]
                [ Html.button
                    [ HE.onClick Generate 
                    , HA.disabled <| Dict.size model.data.user < 2
                    ]
                    [ text "Start new Tournament" ]
                ]
            else text ""
        ]

viewGameBox : Model -> Bool -> Int -> Bracket.KO.Game Int -> Html Msg
viewGameBox model hide scale game =
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
            [ silence model
                <| Html.map WrapGame 
                <| Bracket.KO.viewGame
                <| if hide then Bracket.KO.hideFinishedPhases game else game
            ]
        ]

footer : Html msg
footer =
    div [ class "game-box-footer" ]
        [ text "Proudly made by Max Brauer (Garados007) "
        , Html.a
            [ HA.target "black_"
            , HA.href "https://github.com/Garados007/Tournament"
            ]
            [ text "(Source)" ]
        , text " - \u{00A9} 2021 under GPL 3.0"
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
            then Tuple.pair model
                <| case model.creds of
                    Just (id, Just token) ->
                        Cmd.batch
                        [ Network.wsSend token
                            <| (\data -> { data | origin = model.origin })
                            <| model.data
                        , LocalStorage.setItem storage (id ++ ".data")
                            <| Data.encodeData model.data
                        , LocalStorage.setItem storage (id ++ ".token")
                            <| JE.string token
                        ]
                    _ -> Cmd.none
            else (model, Cmd.none)
        GotBroadcastInfo (Err err) -> Tuple.pair
            { model
            | offline =
                let
                    d_ = Debug.log "error" err
                in True
            }
            Cmd.none
        GotBroadcastInfo (Ok (id, token)) -> Tuple.pair
            { model
            | creds = Just (id, Just token)
            }
            <| Cmd.batch
                [ Browser.Navigation.replaceUrl model.key
                    <| "?id=" ++ Url.percentEncode id
                , Network.wsConnect id
                , LocalStorage.setItem storage (id ++ ".token")
                    <| JE.string token
                , LocalStorage.setItem storage (id ++ ".data")
                    <| Data.encodeData model.data
                ]
        SetOrigin origin -> ({ model | origin = origin }, Cmd.none)
        ReceiveData (Err err) -> Tuple.pair
            { model
            | offline =
                let
                    d_ = Debug.log "error" err
                in model.offline
            }
            Cmd.none
        ReceiveData (Ok data) ->
            if data.origin == model.origin
            then (model, Cmd.none)
            else Tuple.pair { model | data = data } Cmd.none
        GetLocalStorage (LocalStorage.Item key value) ->
            case model.creds of
                Just (id, _) ->
                    if key == id ++ ".token"
                    then Tuple.pair
                        { model
                        | creds = JD.decodeValue JD.string value
                            |> Result.toMaybe
                            |> Maybe.map (Just >> Tuple.pair id >> Just)
                            |> Maybe.withDefault model.creds
                        }
                        Cmd.none
                    else if key == id ++ ".data"
                    then JD.decodeValue Data.decodeData value
                        |> Result.toMaybe
                        |> Maybe.map (\data -> updateData model <| always data)
                        |> Maybe.withDefault (model, Cmd.none)
                    else (model, Cmd.none)
                Nothing -> (model, Cmd.none)
        GetLocalStorage _ -> (model, Cmd.none)
        SocketClose (Ok _) -> Tuple.pair model
            <| Http.get
                { url = "https://relay.2complex.de/api/new"
                , expect = Http.expectJson GotBroadcastInfo
                    <| JD.map2 Tuple.pair
                        (JD.field "id" JD.string)
                        (JD.field "token" JD.string)
                }
        SocketClose (Err err) ->
            let
                d_ = Debug.log "error" err
            in (model, Cmd.none)

