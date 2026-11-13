module Bracket.Knockout.Placement exposing (apply, placements)

{-| The placement slots are round -1. Their index is the place minus one.
-}

import Bracket.Knockout exposing (..)
import Dict
import Placement exposing (Placement)


type alias Rule =
    { round : Int
    , match : Int
    , winner : Bool
    , slot : PlayerSlot
    }


rules : List Rule
rules =
    [ Rule 0 0 True ( -1, 0 )
    , Rule 0 0 False ( -1, 1 )
    , Rule 0 1 True ( -1, 2 )
    ]


{-| Sends the players of the final matches to the placement slots.
-}
apply : Game -> Game
apply game =
    let
        target : Int -> Int -> Bool -> Maybe PlayerSlot
        target round index winner =
            rules
                |> List.filter
                    (\rule ->
                        rule.round
                            == round
                            && rule.match
                            == index
                            && rule.winner
                            == winner
                    )
                |> List.head
                |> Maybe.map .slot
    in
    { game
        | rounds =
            Dict.map
                (\round ->
                    Dict.map <|
                        \index match ->
                            { match
                                | winner =
                                    target round index True
                                        |> Maybe.map Just
                                        |> Maybe.withDefault match.winner
                                , loser =
                                    target round index False
                                        |> Maybe.map Just
                                        |> Maybe.withDefault match.loser
                            }
                )
                game.rounds
    }


{-| The places that some match leads to.
-}
placements : Game -> List Placement
placements game =
    Dict.values game.rounds
        |> List.concatMap Dict.values
        |> List.concatMap (\match -> List.filterMap identity [ match.winner, match.loser ])
        |> List.filter (\( round, _ ) -> round == -1)
        |> List.map Tuple.second
        |> List.sort
        |> List.map
            (\index ->
                { place = index + 1
                , players =
                    Dict.get ( -1, index ) game.slots
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                }
            )
