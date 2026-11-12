module Main exposing (..)

import Bracket.Knockout
import Bracket.Knockout.Placement
import Browser
import Browser.Navigation exposing (Key)
import Data exposing (Data)
import Debug.Extra
import GameBox
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
import Random.String
import Task
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query
import UserPanel


type alias Model =
    { gameBox : GameBox.Model
    , userPanel : UserPanel.Model
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
    | GameBoxMsg GameBox.Msg
    | WrapGame Bracket.Knockout.Msg
    | UserPanelMsg UserPanel.Msg
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
            { gameBox = GameBox.init
            , userPanel = UserPanel.init (id == Nothing)
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
            , Html.map UserPanelMsg <| UserPanel.viewToggler model.userPanel
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
            [ silence model <|
                Html.map UserPanelMsg <|
                    UserPanel.view (canEdit model) model.data model.userPanel
            , Maybe.map (GameBox.view GameBoxMsg (silence model << Html.map WrapGame) model.gameBox) model.data.game
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

        GameBoxMsg sub ->
            ( { model | gameBox = GameBox.update sub model.gameBox }, Cmd.none )

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

        UserPanelMsg sub ->
            let
                ( userPanel, data, cmd ) =
                    UserPanel.update sub model.userPanel model.data
            in
            updateData { model | userPanel = userPanel } (always data)
                |> Tuple.mapSecond
                    (\uploadCmd -> Cmd.batch [ uploadCmd, Cmd.map UserPanelMsg cmd ])

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
