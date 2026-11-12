module Main exposing (..)

import Bracket.Knockout exposing (PlayerId)
import Bracket.Knockout.Placement
import Browser
import Browser.Navigation exposing (Key)
import Data exposing (Data)
import Debug.Extra
import Dict
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Http
import Json.Decode as JD
import Json.Encode as JE
import LocalStorage
import Network
import Ports
import Process
import Random
import Random.Char
import Random.List
import Random.String
import Task
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query


type alias Model =
    { scale : Int
    , userInput : String
    , hide : Bool
    , showUser : Bool
    , data : Data
    , changeset : Int
    , offline : Bool
    , creds : Maybe ( String, Maybe String )
    , key : Key
    , origin : String
    , relay : String
    }


type Msg
    = None
    | SetScale Int
    | WrapGame Bracket.Knockout.Msg
    | SetInput String
    | AddName
    | Generate
    | StartGame (List ( PlayerId, String ))
    | ChangeUserName PlayerId String
    | RemoveUser PlayerId
    | SetHide Bool
    | SetShowUser Bool
    | SetTitle String
    | UploadData Int
    | GotBroadcastInfo (Result Http.Error ( String, String ))
    | SetOrigin String
    | ReceiveData (Result String Data)
    | GetLocalStorage LocalStorage.Response
    | SocketClose (Result JD.Error Network.SocketClose)


type alias Flags =
    { relay : String
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                Browser.Document model.data.title <|
                    view model
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Network.wsReceive ReceiveData
                    , Network.wsClose SocketClose
                    , LocalStorage.responseHandler GetLocalStorage storage
                        |> Ports.settingResponse
                    ]
        , onUrlRequest = always None
        , onUrlChange = always None
        }


requestNewGroup : String -> Cmd Msg
requestNewGroup relay =
    Http.get
        { url = relay ++ "/api/new?replay-last=true"
        , expect =
            Http.expectJson GotBroadcastInfo <|
                JD.map2 Tuple.pair
                    (JD.field "id" JD.string)
                    (JD.field "token" JD.string)
        }


storage : LocalStorage.LocalStorage msg
storage =
    LocalStorage.make
        Ports.settingGetItem
        Ports.settingSetItem
        Ports.settingClear
        Ports.settingListKeys
        "tournament"


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        id : Maybe String
        id =
            { url | path = "" }
                |> Url.Parser.parse
                    (Url.Parser.query <|
                        Url.Parser.Query.string "id"
                    )
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
            , creds = Maybe.map (\x -> ( x, Nothing )) id
            , key = key
            , origin = ""
            , relay = flags.relay
            }

        cmds : List (Cmd Msg)
        cmds =
            [ case id of
                Just id_ ->
                    Network.wsConnect flags.relay id_

                Nothing ->
                    requestNewGroup flags.relay
            , Random.generate SetOrigin <|
                Random.String.string 40 <|
                    Random.Char.latin
            , Maybe.map
                (\id_ -> LocalStorage.getItem storage <| id_ ++ ".token")
                id
                |> Maybe.withDefault Cmd.none
            , Maybe.map
                (\id_ -> LocalStorage.getItem storage <| id_ ++ ".data")
                id
                |> Maybe.withDefault Cmd.none
            ]
    in
    ( model, Cmd.batch cmds )


canEdit : Model -> Bool
canEdit model =
    model.offline
        || (case model.creds of
                Just ( _, Nothing ) ->
                    False

                _ ->
                    True
           )


silence : Model -> Html Msg -> Html Msg
silence model node =
    if canEdit model then
        node

    else
        Html.map (always None) node


view : Model -> List (Html Msg)
view model =
    [ Html.node "link"
        [ HA.attribute "rel" "stylesheet"
        , HA.attribute "property" "stylesheet"
        , HA.attribute "href" "css/root.css"
        ]
        []
    , div [ class "layout" ]
        [ div [ class "title-bar" ]
            [ if canEdit model then
                Html.input
                    [ class "title"
                    , HA.value model.data.title
                    , HE.onInput SetTitle
                    ]
                    []

              else
                div
                    [ class "title" ]
                    [ text model.data.title ]
            , div
                [ HA.classList
                    [ ( "user-toggler", True )
                    , ( "open", model.showUser )
                    ]
                , HE.onClick <| SetShowUser <| not model.showUser
                ]
              <|
                List.repeat 3 <|
                    div [] []
            ]
        , Maybe.withDefault (text "") <|
            Maybe.map
                (\( game, placement ) ->
                    if Bracket.Knockout.Placement.hasPlacement placement game then
                        Bracket.Knockout.Placement.view game placement

                    else
                        text ""
                )
            <|
                Maybe.map2 Tuple.pair model.data.game model.data.placement
        , div [ class "layout-content" ]
            [ div
                [ HA.classList
                    [ ( "layout-user", True )
                    , ( "open", model.showUser )
                    ]
                ]
                [ silence model <| viewUserInput model ]
            , Maybe.map (viewGameBox model model.hide model.scale) model.data.game
                |> Maybe.withDefault
                    (div [ class "layout-replacement" ] <|
                        List.singleton <|
                            text <|
                                if canEdit model then
                                    "There is no tournament yet.\nCreate one."

                                else
                                    "There is no tournament yet.\nWait for one to be created."
                    )
            , footer
            ]
        ]

    -- , Debug.Extra.viewModel <| model
    ]


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


viewUserInput : Model -> Html Msg
viewUserInput model =
    div [ class "user-input" ]
        [ if canEdit model then
            div [ class "user-input-box" ]
                [ Html.input
                    [ HA.type_ "text"
                    , HA.value model.userInput
                    , HE.onInput SetInput
                    , onEnter <|
                        if
                            model.userInput
                                /= ""
                                && not
                                    (List.member model.userInput <|
                                        Dict.values model.data.users
                                    )
                        then
                            AddName

                        else
                            None
                    , HA.placeholder "Enter a name and press Enter"
                    ]
                    []
                ]

          else
            text ""
        , div [ class "user-input-list" ] <|
            List.map
                (\( id, name ) ->
                    if
                        model.data.game
                            |> Maybe.map (Bracket.Knockout.isEliminated id)
                            |> Maybe.withDefault False
                    then
                        div [ class "user-input-entry deleted" ]
                            [ text name ]

                    else
                        div [ class "user-input-entry" ]
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
            <|
                Dict.toList model.data.users
        , if canEdit model then
            div [ class "user-input-start" ]
                [ Html.button
                    [ HE.onClick Generate
                    , HA.disabled <| Dict.size model.data.users < 2
                    ]
                    [ text "Start new tournament" ]
                ]

          else
            text ""
        ]


viewGameBox : Model -> Bool -> Int -> Bracket.Knockout.Game -> Html Msg
viewGameBox model hide scale game =
    div [ class "game-box" ]
        [ div [ class "game-box-controls" ]
            [ Html.button
                [ HE.onClick <| SetHide <| not hide
                , HA.title <|
                    if hide then
                        "Show all matches again"

                    else
                        "Hide finished matches"
                ]
                [ text <|
                    if hide then
                        "🔶"

                    else
                        "🔸"
                ]
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
            , HA.style "font-size" <|
                String.fromFloat (0.75 ^ toFloat scale)
                    ++ "em"
            ]
            [ silence model <|
                Html.map WrapGame <|
                    Bracket.Knockout.viewGame <|
                        if hide then
                            Bracket.Knockout.hideFinishedMatches game

                        else
                            game
            ]
        ]


footer : Html msg
footer =
    div [ class "game-box-footer" ]
        [ text "Proudly made by Max Brauer (Garados007) "
        , Html.a
            [ HA.target "_blank"
            , HA.href "https://github.com/Garados007/Tournament"
            ]
            [ text "(Source)" ]
        , text " - © 2021 under GPL 3.0"
        ]


updateData : Model -> (Data -> Data) -> ( Model, Cmd Msg )
updateData model updater =
    let
        newData =
            updater model.data

        cs =
            model.changeset + 1
    in
    if newData == model.data then
        ( model, Cmd.none )

    else
        Tuple.pair
            { model | data = newData, changeset = cs }
        <|
            Task.perform (always <| UploadData cs) <|
                Process.sleep 100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        SetScale scale ->
            ( { model | scale = scale }, Cmd.none )

        WrapGame sub ->
            case model.data.game of
                Just game ->
                    updateData model <|
                        \data ->
                            { data
                                | game = Just <| Bracket.Knockout.update sub game
                            }

                Nothing ->
                    ( model, Cmd.none )

        SetInput input ->
            ( { model | userInput = input }, Cmd.none )

        AddName ->
            updateData { model | userInput = "" } <|
                \data ->
                    { data
                        | users = Dict.insert data.nextId model.userInput data.users
                        , nextId = data.nextId + 1
                        , game = Maybe.map (Bracket.Knockout.addPlayer data.nextId model.userInput) data.game
                    }

        Generate ->
            updateData model
                (\data ->
                    { data
                        | users =
                            case data.game of
                                Nothing ->
                                    data.users

                                Just game ->
                                    Dict.filter
                                        (\id _ -> not <| Bracket.Knockout.isEliminated id game)
                                        data.users
                    }
                )
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Random.generate StartGame <|
                                Random.List.shuffle <|
                                    (case model.data.game of
                                        Nothing ->
                                            identity

                                        Just game ->
                                            List.filter <|
                                                \( id, _ ) -> not <| Bracket.Knockout.isEliminated id game
                                    )
                                    <|
                                        Dict.toList model.data.users
                            ]
                    )

        StartGame list ->
            updateData { model | showUser = False } <|
                \data ->
                    let
                        ( game, placement ) =
                            Bracket.Knockout.Placement.getPlacement <|
                                Bracket.Knockout.start list
                    in
                    { data
                        | game = Just game
                        , placement = Just placement
                    }

        ChangeUserName id name ->
            updateData model <|
                \data ->
                    { data
                        | users = Dict.insert id name data.users
                        , game = Maybe.map (Bracket.Knockout.renamePlayer id name) data.game
                    }

        RemoveUser id ->
            updateData model <|
                \data ->
                    case data.game of
                        Nothing ->
                            { data | users = Dict.remove id data.users }

                        Just game ->
                            { data | game = Just <| Bracket.Knockout.removePlayer id game }

        SetHide hide ->
            ( { model | hide = hide }, Cmd.none )

        SetShowUser show ->
            ( { model | showUser = show }, Cmd.none )

        SetTitle title ->
            updateData model <| \data -> { data | title = title }

        UploadData changeset ->
            if model.changeset == changeset then
                Tuple.pair model <|
                    case model.creds of
                        Just ( id, Just token ) ->
                            Cmd.batch
                                [ Network.wsSend token <|
                                    (\data -> { data | origin = model.origin }) <|
                                        model.data
                                , LocalStorage.setItem storage (id ++ ".data") <|
                                    Data.encodeData model.data
                                , LocalStorage.setItem storage (id ++ ".token") <|
                                    JE.string token
                                ]

                        _ ->
                            Cmd.none

            else
                ( model, Cmd.none )

        GotBroadcastInfo (Err _) ->
            ( { model | offline = True }, Cmd.none )

        GotBroadcastInfo (Ok ( id, token )) ->
            Tuple.pair
                { model
                    | creds = Just ( id, Just token )
                }
            <|
                Cmd.batch
                    [ Browser.Navigation.replaceUrl model.key <|
                        "?id="
                            ++ Url.percentEncode id
                    , Network.wsConnect model.relay id
                    , LocalStorage.setItem storage (id ++ ".token") <|
                        JE.string token
                    , LocalStorage.setItem storage (id ++ ".data") <|
                        Data.encodeData model.data

                    -- a new group starts empty, so viewers would see nothing until the next edit
                    , Task.perform (always <| UploadData model.changeset) <|
                        Task.succeed ()
                    ]

        SetOrigin origin ->
            ( { model | origin = origin }, Cmd.none )

        ReceiveData (Err _) ->
            ( model, Cmd.none )

        ReceiveData (Ok data) ->
            if data.origin == model.origin then
                ( model, Cmd.none )

            else
                Tuple.pair { model | data = data } Cmd.none

        GetLocalStorage (LocalStorage.Item key value) ->
            case model.creds of
                Just ( id, _ ) ->
                    if key == id ++ ".token" then
                        Tuple.pair
                            { model
                                | creds =
                                    JD.decodeValue JD.string value
                                        |> Result.toMaybe
                                        |> Maybe.map (Just >> Tuple.pair id >> Just)
                                        |> Maybe.withDefault model.creds
                            }
                            Cmd.none

                    else if key == id ++ ".data" then
                        JD.decodeValue Data.decodeData value
                            |> Result.toMaybe
                            |> Maybe.map (\data -> updateData model <| always data)
                            |> Maybe.withDefault ( model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GetLocalStorage _ ->
            ( model, Cmd.none )

        SocketClose (Ok _) ->
            ( model, requestNewGroup model.relay )

        SocketClose (Err _) ->
            ( model, Cmd.none )
