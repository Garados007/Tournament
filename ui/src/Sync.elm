module Sync exposing (Model, Msg, canEdit, changed, groupFromUrl, init, subscriptions, update)

{-| Shares the data through the relay and keeps a copy in localStorage.
-}

import Browser.Navigation exposing (Key)
import Data exposing (Data)
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
    }


type Msg
    = UploadData Int
    | GotBroadcastInfo (Result Http.Error ( String, String ))
    | SetOrigin String
    | ReceiveData (Result String Data)
    | GetLocalStorage LocalStorage.Response
    | SocketClose (Result JD.Error Network.SocketClose)


groupFromUrl : Url -> Maybe String
groupFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.query <|
                Url.Parser.Query.string "id"
            )
        |> Maybe.andThen identity


{-| Joins the given group, or creates a new one without a group.
-}
init : String -> Key -> Maybe String -> ( Model, Cmd Msg )
init relay key id =
    ( { changeset = 0
      , offline = False
      , creds = Maybe.map (\x -> ( x, Nothing )) id
      , key = key
      , origin = ""
      , relay = relay
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


canEdit : Model -> Bool
canEdit model =
    model.offline
        || (case model.creds of
                Just ( _, Nothing ) ->
                    False

                _ ->
                    True
           )


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
            ( model, Nothing, Cmd.none )

        ReceiveData (Ok received) ->
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
                        case JD.decodeValue Data.decodeData value of
                            Ok stored ->
                                if stored == data then
                                    ( model, Nothing, Cmd.none )

                                else
                                    let
                                        ( newModel, cmd ) =
                                            changed model
                                    in
                                    ( newModel, Just stored, cmd )

                            Err _ ->
                                ( model, Nothing, Cmd.none )

                    else
                        ( model, Nothing, Cmd.none )

                Nothing ->
                    ( model, Nothing, Cmd.none )

        GetLocalStorage _ ->
            ( model, Nothing, Cmd.none )

        SocketClose (Ok _) ->
            ( model, Nothing, requestNewGroup model.relay )

        SocketClose (Err _) ->
            ( model, Nothing, Cmd.none )
