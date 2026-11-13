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
import Sync
import Url exposing (Url)
import UserPanel


type alias Model =
    { gameBox : GameBox.Model
    , userPanel : UserPanel.Model
    , data : Data
    , sync : Sync.Model
    }


type Msg
    = None
    | GameBoxMsg GameBox.Msg
    | WrapGame Bracket.Knockout.Msg
    | UserPanelMsg UserPanel.Msg
    | SetTitle String
    | SyncMsg Sync.Msg


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
            Sync.init flags.relay key id
    in
    ( { gameBox = GameBox.init
      , userPanel = UserPanel.init (id == Nothing)
      , data = Data.init
      , sync = sync
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
                    UserPanel.view (Sync.canEdit model.sync) model.data model.userPanel
            , Maybe.map (GameBox.view GameBoxMsg (silence model << Html.map WrapGame) model.gameBox) model.data.game
                |> Maybe.withDefault
                    (div [ class "layout-replacement" ] <|
                        List.singleton <|
                            text <|
                                if Sync.canEdit model.sync then
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
