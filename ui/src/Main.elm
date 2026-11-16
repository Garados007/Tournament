module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Data exposing (Data)
import Debug.Extra
import GameBox
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Sync
import Tournament
import Transfer
import Url exposing (Url)
import UserPanel


type alias Model =
    { gameBox : GameBox.Model
    , userPanel : UserPanel.Model
    , data : Data
    , sync : Sync.Model
    , local : Tournament.Local
    , transfer : Transfer.Model
    }


type Msg
    = None
    | GameBoxMsg GameBox.Msg
    | TournamentMsg Tournament.Msg
    | UserPanelMsg UserPanel.Msg
    | SetTitle String
    | SyncMsg Sync.Msg
    | TransferMsg Transfer.Msg


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
        , subscriptions = always <| Sub.map SyncMsg Sync.subscriptions
        , onUrlRequest = always None
        , onUrlChange = always None
        }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        id : Maybe String
        id =
            Sync.groupFromUrl url

        ( sync, cmd ) =
            Sync.init flags.relay key url
    in
    ( { gameBox = GameBox.init
      , userPanel = UserPanel.init (id == Nothing)
      , data = Data.init
      , sync = sync
      , local = Tournament.initLocal
      , transfer = Transfer.init
      }
    , Cmd.map SyncMsg cmd
    )


silence : Model -> Html Msg -> Html Msg
silence model node =
    if Sync.canEdit model.sync then
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
            [ if Sync.canEdit model.sync then
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
        , model.data.tournament
            |> Maybe.map (Tournament.viewPlacements model.data.users)
            |> Maybe.withDefault (text "")
        , div [ class "layout-content" ]
            [ silence model <|
                Html.map UserPanelMsg <|
                    UserPanel.view (Sync.canEdit model.sync) model.data model.userPanel
            , model.data.tournament
                |> Maybe.map
                    (\tournament ->
                        GameBox.view GameBoxMsg
                            (Tournament.controls tournament)
                            { buttons =
                                Transfer.viewButtons
                                    { canEdit = Sync.canEdit model.sync
                                    , csv =
                                        Tournament.csv tournament
                                            |> Maybe.map (always True)
                                            |> Maybe.withDefault False
                                    }
                                    |> List.map (Html.map TransferMsg)
                            , content =
                                \box ->
                                    Tournament.view
                                        { names = model.data.users
                                        , box = box
                                        , canEdit = Sync.canEdit model.sync
                                        }
                                        model.local
                                        tournament
                                        |> Html.map
                                            (\sub ->
                                                if Sync.canEdit model.sync || Tournament.isViewerMsg sub then
                                                    TournamentMsg sub

                                                else
                                                    None
                                            )
                            }
                            model.gameBox
                    )
                |> Maybe.withDefault
                    (div [ class "layout-replacement" ] <|
                        if Sync.canEdit model.sync then
                            [ text "There is no tournament yet.\nCreate one."
                            , Html.map TransferMsg Transfer.importButton
                            ]

                        else
                            [ text "There is no tournament yet.\nWait for one to be created." ]
                    )
            , footer
            ]
        ]
    , Html.map TransferMsg <| Transfer.view model.transfer
    , Html.map SyncMsg <| Sync.viewProblem model.sync

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
    in
    if newData == model.data then
        ( model, Cmd.none )

    else
        let
            ( sync, cmd ) =
                Sync.changed model.sync
        in
        ( { model | data = newData, sync = sync }, Cmd.map SyncMsg cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        GameBoxMsg sub ->
            ( { model | gameBox = GameBox.update sub model.gameBox }, Cmd.none )

        TournamentMsg sub ->
            case model.data.tournament of
                Just tournament ->
                    let
                        ( local, newTournament ) =
                            Tournament.update sub model.local tournament
                    in
                    updateData { model | local = local } <|
                        \data -> { data | tournament = Just newTournament }

                Nothing ->
                    ( model, Cmd.none )

        UserPanelMsg sub ->
            let
                ( userPanel, data, cmd ) =
                    UserPanel.update sub model.userPanel model.data

                -- drafts of the old tournament would show up in a new one with the same players
                local =
                    case sub of
                        UserPanel.StartGame _ ->
                            Tournament.initLocal

                        UserPanel.Generate ->
                            Tournament.initLocal

                        _ ->
                            model.local
            in
            updateData { model | userPanel = userPanel, local = local } (always data)
                |> Tuple.mapSecond
                    (\uploadCmd -> Cmd.batch [ uploadCmd, Cmd.map UserPanelMsg cmd ])

        SetTitle title ->
            updateData model <| \data -> { data | title = title }

        TransferMsg sub ->
            let
                ( transfer, imported, cmd ) =
                    Transfer.update { canEdit = Sync.canEdit model.sync } sub model.data model.transfer

                newModel =
                    { model | transfer = transfer }
            in
            case imported of
                Just data ->
                    -- drafts of the old tournament would show up in the imported one
                    updateData { newModel | local = Tournament.initLocal } (always data)
                        |> Tuple.mapSecond (\syncCmd -> Cmd.batch [ syncCmd, Cmd.map TransferMsg cmd ])

                Nothing ->
                    ( newModel, Cmd.map TransferMsg cmd )

        SyncMsg sub ->
            let
                ( sync, data, cmd ) =
                    Sync.update sub model.data model.sync
            in
            ( { model
                | sync = sync
                , data = Maybe.withDefault model.data data
              }
            , Cmd.map SyncMsg cmd
            )
