module Placement exposing (Placement, view)

{-| The banner with the final places, shared by all modes.
-}

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import PlayerId exposing (PlayerId)


{-| Several players share a place if the list has more than one entry. An empty
list means the place isn't decided yet.
-}
type alias Placement =
    { place : Int
    , players : List PlayerId
    }


suffix : Int -> String
suffix place =
    if modBy 100 place >= 11 && modBy 100 place <= 13 then
        "th"

    else
        case modBy 10 place of
            1 ->
                "st"

            2 ->
                "nd"

            3 ->
                "rd"

            _ ->
                "th"


{-| Shows nothing until at least one place is decided.
-}
view : { names : Dict PlayerId String, isEliminated : PlayerId -> Bool } -> List Placement -> Html msg
view config placements =
    if List.all (.players >> List.isEmpty) placements then
        text ""

    else
        div [ class "placements" ] <|
            List.map
                (\placement ->
                    div [ class "placement" ]
                        [ div [ class "place" ]
                            [ div [ class "big" ]
                                [ text <| String.fromInt placement.place ]
                            , div [ class "small" ]
                                [ text <| suffix placement.place ]
                            ]
                        , div [ class "user" ] <|
                            List.map
                                (\id ->
                                    div
                                        [ HA.classList
                                            [ ( "player", True )
                                            , ( "alive", not <| config.isEliminated id )
                                            ]
                                        ]
                                        [ text <| Maybe.withDefault "" <| Dict.get id config.names ]
                                )
                                placement.players
                        ]
                )
                placements
