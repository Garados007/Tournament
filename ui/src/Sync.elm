module Sync exposing
    ( CloseAction(..)
    , Model
    , Msg
    , Problem(..)
    , Role(..)
    , Stored(..)
    , canEdit
    , changed
    , closeAction
    , decodeStored
    , groupFromUrl
    , init
    , report
    , role
    , subscriptions
    , update
    , viewProblem
    )

{-| Shares the data through the relay and keeps a copy in localStorage.
-}

import Browser.Navigation exposing (Key)
import Data exposing (Data)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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


type alias Model =
    { changeset : Int
    , offline : Bool
    , creds : Maybe ( String, Maybe String )
    , key : Key
    , origin : String
    , relay : String
    , home : String
    , problem : Maybe Problem
    }


type Problem
    = StoredDataInvalid
    | ReceivedDataInvalid
    | ConnectionLost


type Role
    = Editor
    | Viewer


type CloseAction
    = HostNewGroup
    | ReportConnectionLost


type Stored
    = NothingStored
    | Stored Data
    | Unreadable


type Msg
    = UploadData Int
    | GotBroadcastInfo (Result Http.Error ( String, String ))
    | SetOrigin String
    | ReceiveData (Result String (Maybe Data))
    | GetLocalStorage LocalStorage.Response
    | SocketClose (Result JD.Error Network.SocketClose)
    | ResetAndReload
    | Reload
    | StartNew
    | HostCopy


groupFromUrl : Url -> Maybe String
groupFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.query <|
                Url.Parser.Query.string "id"
            )
        |> Maybe.andThen identity


{-| Joins the group from the URL, or creates a new one without a group.
-}
init : String -> Key -> Url -> ( Model, Cmd Msg )
init relay key url =
    let
        id =
            groupFromUrl url
    in
    ( { changeset = 0
      , offline = False
      , creds = Maybe.map (\x -> ( x, Nothing )) id
      , key = key
      , origin = ""
      , relay = relay
      , home = Url.toString { url | query = Nothing, fragment = Nothing }
      , problem = Nothing
      }
    , Cmd.batch
        [ case id of
            Just id_ ->
                Network.wsConnect relay id_

            Nothing ->
                requestNewGroup relay
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
    )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Network.wsReceive ReceiveData
        , Network.wsClose SocketClose
        , LocalStorage.responseHandler GetLocalStorage storage
            |> Ports.settingResponse
        ]


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


role : { a | offline : Bool, creds : Maybe ( String, Maybe String ) } -> Role
role model =
    case ( model.offline, model.creds ) of
        ( False, Just ( _, Nothing ) ) ->
            Viewer

        _ ->
            Editor


canEdit : Model -> Bool
canEdit model =
    role model == Editor


{-| What to do when the relay deleted the group. Editors have the data to fill
a new group right away; viewers are asked first.
-}
closeAction : Role -> CloseAction
closeAction r =
    case r of
        Editor ->
            HostNewGroup

        Viewer ->
            ReportConnectionLost


{-| Only the problem with the strongest remedy is shown: a reset also fixes
what a reload fixes.
-}
report : Problem -> Maybe Problem -> Maybe Problem
report new current =
    case current of
        Just old ->
            if severity old >= severity new then
                Just old

            else
                Just new

        Nothing ->
            Just new


severity : Problem -> Int
severity problem =
    case problem of
        StoredDataInvalid ->
            3

        ReceivedDataInvalid ->
            2

        ConnectionLost ->
            1


{-| `null` means nothing is saved.
-}
decodeStored : JD.Value -> Stored
decodeStored value =
    case JD.decodeValue (JD.nullable Data.decodeData) value of
        Ok (Just data) ->
            Stored data

        Ok Nothing ->
            NothingStored

        Err _ ->
            Unreadable


{-| Schedules the upload of changed data. Of several changes in a short time
only the last one is uploaded.
-}
changed : Model -> ( Model, Cmd Msg )
changed model =
    let
        cs =
            model.changeset + 1
    in
    ( { model | changeset = cs }
    , Task.perform (always <| UploadData cs) <|
        Process.sleep 100
    )


{-| Returns the new data if it was received or loaded.
-}
update : Msg -> Data -> Model -> ( Model, Maybe Data, Cmd Msg )
update msg data model =
    case msg of
        UploadData changeset ->
            if model.changeset == changeset then
                ( model
                , Nothing
                , case model.creds of
                    Just ( id, Just token ) ->
                        Cmd.batch
                            [ Network.wsSend token { data | origin = model.origin }
                            , LocalStorage.setItem storage (id ++ ".data") <|
                                Data.encodeData data
                            , LocalStorage.setItem storage (id ++ ".token") <|
                                JE.string token
                            ]

                    _ ->
                        Cmd.none
                )

            else
                ( model, Nothing, Cmd.none )

        GotBroadcastInfo (Err _) ->
            ( { model | offline = True }, Nothing, Cmd.none )

        GotBroadcastInfo (Ok ( id, token )) ->
            ( { model | creds = Just ( id, Just token ) }
            , Nothing
            , Cmd.batch
                [ Browser.Navigation.replaceUrl model.key <|
                    "?id="
                        ++ Url.percentEncode id
                , Network.wsConnect model.relay id
                , LocalStorage.setItem storage (id ++ ".token") <|
                    JE.string token
                , LocalStorage.setItem storage (id ++ ".data") <|
                    Data.encodeData data

                -- a new group starts empty, so viewers would see nothing until the next edit
                , Task.perform (always <| UploadData model.changeset) <|
                    Task.succeed ()
                ]
            )

        SetOrigin origin ->
            ( { model | origin = origin }, Nothing, Cmd.none )

        ReceiveData (Err _) ->
            ( { model | problem = report ReceivedDataInvalid model.problem }
            , Nothing
            , Cmd.none
            )

        ReceiveData (Ok Nothing) ->
            ( model, Nothing, Cmd.none )

        ReceiveData (Ok (Just received)) ->
            if received.origin == model.origin then
                ( model, Nothing, Cmd.none )

            else
                ( model, Just received, Cmd.none )

        GetLocalStorage (LocalStorage.Item key value) ->
            case model.creds of
                Just ( id, _ ) ->
                    if key == id ++ ".token" then
                        ( { model
                            | creds =
                                JD.decodeValue JD.string value
                                    |> Result.toMaybe
                                    |> Maybe.map (Just >> Tuple.pair id >> Just)
                                    |> Maybe.withDefault model.creds
                          }
                        , Nothing
                        , Cmd.none
                        )

                    else if key == id ++ ".data" then
                        case decodeStored value of
                            Stored stored ->
                                if stored == data then
                                    ( model, Nothing, Cmd.none )

                                else
                                    let
                                        ( newModel, cmd ) =
                                            changed model
                                    in
                                    ( newModel, Just stored, cmd )

                            NothingStored ->
                                ( model, Nothing, Cmd.none )

                            Unreadable ->
                                ( { model | problem = report StoredDataInvalid model.problem }
                                , Nothing
                                , Cmd.none
                                )

                    else
                        ( model, Nothing, Cmd.none )

                Nothing ->
                    ( model, Nothing, Cmd.none )

        GetLocalStorage _ ->
            ( model, Nothing, Cmd.none )

        SocketClose (Ok _) ->
            case closeAction (role model) of
                HostNewGroup ->
                    ( model, Nothing, requestNewGroup model.relay )

                ReportConnectionLost ->
                    ( { model | problem = report ConnectionLost model.problem }
                    , Nothing
                    , Cmd.none
                    )

        SocketClose (Err _) ->
            ( model, Nothing, Cmd.none )

        ResetAndReload ->
            ( model
            , Nothing
            , Cmd.batch
                [ Ports.settingClear ""
                , Browser.Navigation.load model.home
                ]
            )

        Reload ->
            ( model, Nothing, Browser.Navigation.reload )

        StartNew ->
            ( model, Nothing, Browser.Navigation.load model.home )

        HostCopy ->
            ( { model | problem = Nothing }, Nothing, requestNewGroup model.relay )


viewProblem : Model -> Html Msg
viewProblem model =
    case model.problem of
        Nothing ->
            text ""

        Just problem ->
            let
                ( message, buttons ) =
                    case problem of
                        StoredDataInvalid ->
                            ( "The saved tournament can't be loaded."
                            , [ ( ResetAndReload, "Reset and reload" ) ]
                            )

                        ReceivedDataInvalid ->
                            ( "The received tournament can't be read. The app needs to be reloaded."
                            , [ ( Reload, "Reload" ) ]
                            )

                        ConnectionLost ->
                            ( "The connection to the tournament was lost."
                            , [ ( StartNew, "Start a new tournament" )
                              , ( HostCopy, "Host a copy" )
                              ]
                            )
            in
            div [ class "problem-overlay" ]
                [ div [ class "problem" ]
                    [ div [ class "problem-message" ] [ text message ]
                    , buttons
                        |> List.map
                            (\( msg, label ) ->
                                button [ onClick msg ] [ text label ]
                            )
                        |> div [ class "problem-buttons" ]
                    ]
                ]
