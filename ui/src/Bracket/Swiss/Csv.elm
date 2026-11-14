module Bracket.Swiss.Csv exposing (table)

{-| The standings of a Swiss tournament for a spreadsheet program.
-}

import Bracket.Scoring exposing (Scoring(..))
import Bracket.Swiss as Swiss exposing (State)
import Dict exposing (Dict)
import PlayerId exposing (PlayerId)


{-| Rows of cells, the header first. Scores like `3 : 1` have spaces, so a
spreadsheet program doesn't read them as a time.
-}
table : Dict PlayerId String -> State -> List (List String)
table names state =
    let
        withPoints =
            state.settings.scoring == Points

        optional value =
            if withPoints then
                [ value ]

            else
                []

        name id =
            Maybe.withDefault "" <| Dict.get id names
    in
    ([ "Place", "Name", "W", "D", "L" ] ++ optional "Score" ++ [ "Pts", "Buchholz" ])
        :: List.map
            (\standing ->
                [ Maybe.withDefault "" <| Maybe.map String.fromInt standing.place
                , name standing.id
                ]
                    ++ List.map String.fromInt [ standing.wins, standing.draws, standing.losses ]
                    ++ optional (String.fromInt standing.scored ++ " : " ++ String.fromInt standing.conceded)
                    ++ List.map String.fromInt [ standing.matchPoints, standing.buchholz ]
            )
            (Swiss.standings state)
