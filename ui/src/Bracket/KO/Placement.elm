module Bracket.KO.Placement exposing (..)

import Bracket.KO exposing (..)
import Bracket.KO.Serializer
import Dict
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE

type alias Placements = List Placement

type alias Placement =
    { bigLetter: String
    , smallLetter: String
    , slot: PlayerSlot
    }

decode : Decoder Placements
decode =
    JD.succeed Placement
    |> required "bigLetter" JD.string
    |> required "smallLetter" JD.string
    |> required "slot" Bracket.KO.Serializer.decodePlayerSlot
    |> JD.list

encode : Placements -> Value
encode =
    JE.list
    <| \placement ->
        JE.object
            [ ("bigLetter", JE.string placement.bigLetter)
            , ("smallLetter", JE.string placement.smallLetter)
            , ("slot", Bracket.KO.Serializer.encodePlayerSlot placement.slot)
            ]

type alias Rule =
    { phase: Int
    , level: Int
    , winner: Bool
    , placement: Placement
    }

rules : List Rule
rules =
    [ Rule 0 0 True <| Placement "1" "st" (-1, 0)
    , Rule 0 0 False <| Placement "2" "nd" (-1, 1)
    , Rule 0 1 True <| Placement "3" "rd" (-1, 2)
    ]

getPlacement : Game id -> (Game id, Placements)
getPlacement game =
    let
        validRules : List Rule
        validRules = rules
            |> List.filter
                (\rule ->
                    Dict.get rule.phase game.levels
                    |> Maybe.andThen (Dict.get rule.level)
                    |> (/=) Nothing
                )
        
        appliedGame : Game id
        appliedGame =
            { game
            | levels =
                Dict.map
                    (\phase ->
                        Dict.map
                        <| \level data ->
                            { data
                            | winner = validRules
                                |> List.filter
                                    (\rule -> rule.phase == phase
                                        && rule.level == level
                                        && rule.winner
                                    )
                                |> List.head
                                |> Maybe.map (Just << .slot << .placement)
                                |> Maybe.withDefault data.winner
                            , looser = validRules
                                |> List.filter
                                    (\rule -> rule.phase == phase
                                        && rule.level == level
                                        && not rule.winner
                                    )
                                |> List.head
                                |> Maybe.map (Just << .slot << .placement)
                                |> Maybe.withDefault data.looser
                            }
                    )
                    game.levels
            }
    in (appliedGame, List.map .placement validRules)

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
                        [ text placement.bigLetter ]
                    , div [ class "small" ]
                        [ text placement.smallLetter ]
                    ]
                , div [ class "user" ]
                    <| Maybe.withDefault []
                    <| Maybe.map List.singleton
                    <| Maybe.map
                        (\player ->
                            div
                                [ HA.classList
                                    [ ("player", True)
                                    , ("alive", player.alive)
                                    ]
                                ]
                                [ text player.name ]
                        )
                    <| Maybe.andThen
                        (\id -> Dict.get id game.player)
                    <| Dict.get placement.slot game.slots
                ]
        )
