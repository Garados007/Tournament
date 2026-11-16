module Bracket.RoundRobin.Csv exposing (table)

{-| The table of a round robin as on the screen, for a spreadsheet program.
-}

import Bracket.RoundRobin as RR exposing (Ranking(..), State)
import Bracket.Scoring as Scoring exposing (Cell(..), Outcome(..), Scoring(..))
import Dict exposing (Dict)
import PlayerId exposing (PlayerId)


{-| Rows of cells, the header first. Results like `3 : 1` have spaces, so a
spreadsheet program doesn't read them as a time.
-}
table : Dict PlayerId String -> State -> List (List String)
table names state =
    let
        players =
            state.game.players

        withPoints =
            state.settings.scoring == Points

        withMatchPoints =
            RR.effectiveRanking state.settings == ByResults

        name id =
            Maybe.withDefault "" <| Dict.get id names

        standings =
            RR.standings state
                |> List.map (\standing -> ( standing.id, standing ))
                |> Dict.fromList

        optional condition value =
            if condition then
                [ value ]

            else
                []

        header =
            ""
                :: List.map (.id >> name) players
                ++ [ "W", "D", "L" ]
                ++ optional withPoints "Score"
                ++ optional withMatchPoints "Pts"
                ++ [ "Place" ]

        row player =
            name player.id
                :: List.map
                    (\column ->
                        if column.id == player.id then
                            ""

                        else
                            cellText state.settings.scoring <| RR.cell player.id column.id state
                    )
                    players
                ++ (case Dict.get player.id standings of
                        Just standing ->
                            List.map String.fromInt [ standing.wins, standing.draws, standing.losses ]
                                ++ optional withPoints (String.fromInt standing.scored ++ " : " ++ String.fromInt standing.conceded)
                                ++ optional withMatchPoints (String.fromInt standing.matchPoints)
                                ++ [ Maybe.withDefault "" <| Maybe.map String.fromInt standing.place ]

                        Nothing ->
                            []
                   )
    in
    header :: List.map row players


cellText : Scoring -> Cell -> String
cellText scoring c =
    case ( c, scoring ) of
        ( Played ( a, b ), Points ) ->
            String.fromInt a ++ " : " ++ String.fromInt b

        ( Played _, WinnerOnly ) ->
            case Scoring.cellOutcome c of
                Just Win ->
                    "W"

                Just Draw ->
                    "D"

                Just Loss ->
                    "L"

                Nothing ->
                    ""

        ( Forfeit _, _ ) ->
            "w/o"

        ( Cancelled, _ ) ->
            ""

        ( Open, _ ) ->
            ""
