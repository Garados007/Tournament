module Bracket.Knockout.Placement exposing (..)

import Bracket.Knockout exposing (..)
import Bracket.Knockout.Serializer
import Dict
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE


type alias Placements =
    List Placement


type alias Placement =
    { number : String
    , suffix : String
    , slot : PlayerSlot
    }


decode : Decoder Placements
decode =
    JD.succeed Placement
        |> required "number" JD.string
        |> required "suffix" JD.string
        |> required "slot" Bracket.Knockout.Serializer.decodePlayerSlot
        |> JD.list


encode : Placements -> Value
encode =
    JE.list <|
        \placement ->
            JE.object
                [ ( "number", JE.string placement.number )
                , ( "suffix", JE.string placement.suffix )
                , ( "slot", Bracket.Knockout.Serializer.encodePlayerSlot placement.slot )
                ]


type alias Rule =
    { round : Int
    , match : Int
    , winner : Bool
    , placement : Placement
    }


rules : List Rule
rules =
    [ Rule 0 0 True <| Placement "1" "st" ( -1, 0 )
    , Rule 0 0 False <| Placement "2" "nd" ( -1, 1 )
    , Rule 0 1 True <| Placement "3" "rd" ( -1, 2 )
    ]


getPlacement : Game id -> ( Game id, Placements )
getPlacement game =
    let
        validRules : List Rule
        validRules =
            rules
                |> List.filter
                    (\rule ->
                        Dict.get rule.round game.rounds
                            |> Maybe.andThen (Dict.get rule.match)
                            |> (/=) Nothing
                    )

        appliedGame : Game id
        appliedGame =
            { game
                | rounds =
                    Dict.map
                        (\round ->
                            Dict.map <|
                                \index match ->
                                    { match
                                        | winner =
                                            validRules
                                                |> List.filter
                                                    (\rule ->
                                                        rule.round
                                                            == round
                                                            && rule.match
                                                            == index
                                                            && rule.winner
                                                    )
                                                |> List.head
                                                |> Maybe.map (Just << .slot << .placement)
                                                |> Maybe.withDefault match.winner
                                        , loser =
                                            validRules
                                                |> List.filter
                                                    (\rule ->
                                                        rule.round
                                                            == round
                                                            && rule.match
                                                            == index
                                                            && not rule.winner
                                                    )
                                                |> List.head
                                                |> Maybe.map (Just << .slot << .placement)
                                                |> Maybe.withDefault match.loser
                                    }
                        )
                        game.rounds
            }
    in
    ( appliedGame, List.map .placement validRules )


hasPlacement : Placements -> Game id -> Bool
hasPlacement placements { slots } =
    List.any
        (\{ slot } ->
            Dict.get slot slots /= Nothing
        )
        placements


view : Game comparableId -> Placements -> Html msg
view game =
    div [ class "placements" ]
        << List.map
            (\placement ->
                div [ class "placement" ]
                    [ div [ class "place" ]
                        [ div [ class "big" ]
                            [ text placement.number ]
                        , div [ class "small" ]
                            [ text placement.suffix ]
                        ]
                    , div [ class "user" ] <|
                        Maybe.withDefault [] <|
                            Maybe.map List.singleton <|
                                Maybe.map
                                    (\player ->
                                        div
                                            [ HA.classList
                                                [ ( "player", True )
                                                , ( "alive", player.alive )
                                                ]
                                            ]
                                            [ text player.name ]
                                    )
                                <|
                                    Maybe.andThen
                                        (\id -> Dict.get id game.players)
                                    <|
                                        Dict.get placement.slot game.slots
                    ]
            )
