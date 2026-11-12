module UserPanel exposing (Model, Msg, init, update, view, viewToggler)

import Bracket.Knockout exposing (PlayerId)
import Bracket.Knockout.Placement
import Data exposing (Data)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Json.Decode as JD
import Random
import Random.List


type alias Model =
    { input : String
    , open : Bool
    }


type Msg
    = SetInput String
    | AddName
    | Generate
    | StartGame (List ( PlayerId, String ))
    | ChangeUserName PlayerId String
    | RemoveUser PlayerId
    | SetOpen Bool


init : Bool -> Model
init open =
    { input = ""
    , open = open
    }


canAdd : Data -> String -> Bool
canAdd data name =
    name /= "" && not (List.member name <| Dict.values data.users)


update : Msg -> Model -> Data -> ( Model, Data, Cmd Msg )
update msg model data =
    case msg of
        SetInput input ->
            ( { model | input = input }, data, Cmd.none )

        AddName ->
            if canAdd data model.input then
                ( { model | input = "" }
                , { data
                    | users = Dict.insert data.nextId model.input data.users
                    , nextId = data.nextId + 1
                    , game = Maybe.map (Bracket.Knockout.addPlayer data.nextId model.input) data.game
                  }
                , Cmd.none
                )

            else
                ( model, data, Cmd.none )

        Generate ->
            let
                users =
                    case data.game of
                        Nothing ->
                            data.users

                        Just game ->
                            Dict.filter
                                (\id _ -> not <| Bracket.Knockout.isEliminated id game)
                                data.users
            in
            ( model
            , { data | users = users }
            , Random.generate StartGame <|
                Random.List.shuffle <|
                    Dict.toList users
            )

        StartGame list ->
            let
                ( game, placement ) =
                    Bracket.Knockout.Placement.getPlacement <|
                        Bracket.Knockout.start list
            in
            ( { model | open = False }
            , { data
                | game = Just game
                , placement = Just placement
              }
            , Cmd.none
            )

        ChangeUserName id name ->
            ( model
            , { data
                | users = Dict.insert id name data.users
                , game = Maybe.map (Bracket.Knockout.renamePlayer id name) data.game
              }
            , Cmd.none
            )

        RemoveUser id ->
            ( model
            , case data.game of
                Nothing ->
                    { data | users = Dict.remove id data.users }

                Just game ->
                    { data | game = Just <| Bracket.Knockout.removePlayer id game }
            , Cmd.none
            )

        SetOpen open ->
            ( { model | open = open }, data, Cmd.none )


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
                text ""
            , div [ class "user-input-list" ] <|
                List.map
                    (\( id, name ) ->
                        if
                            data.game
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
                    Dict.toList data.users
            , if canEdit then
                div [ class "user-input-start" ]
                    [ Html.button
                        [ HE.onClick Generate
                        , HA.disabled <| Dict.size data.users < 2
                        ]
                        [ text "Start new tournament" ]
                    ]

              else
                text ""
            ]
        ]
