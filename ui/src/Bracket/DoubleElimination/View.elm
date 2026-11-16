module Bracket.DoubleElimination.View exposing (Layout, Position, layout, matchName, position, source, view)

{-| The winner bracket on top, the loser bracket below, both in the knockout
style. The grand final and the reset match follow at the end.

The grid has a column for the matches and a narrow one for the lines,
alternating. Both brackets share the columns: every loser bracket round is right
of the winner bracket rounds it receives players from, and every winner bracket
round is as far right as that allows. So the losers of a winner bracket round
drop into the next column.

-}

import Bitwise
import Bracket.BestOf exposing (BestOf)
import Bracket.DoubleElimination as DE exposing (State)
import Bracket.Knockout as Knockout exposing (Game, HighlightMsg(..), Match, MatchKey, Msg(..), Path, PlayerSlot)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import List.Extra
import PlayerId exposing (PlayerId)
import Set


{-| `k` is the number of winner bracket rounds of the bracket filled up to a
power of two. `loserTop` is the first row of the loser bracket.
-}
type alias Layout =
    { k : Int
    , loserTop : Int
    , rows : Int
    , finalColumn : Int
    , finalCenter : Int
    }


{-| Where a match box is: its column and the row in its middle. It spans one
row above and below.
-}
type alias Position =
    { column : Int
    , center : Int
    }


power : Int -> Int
power n =
    Bitwise.shiftLeftBy (max 0 n) 1


{-| The round of the winner bracket, counted from 1 for its first round.
-}
winnerRoundNumber : Layout -> Int -> Int
winnerRoundNumber { k } round =
    k - round + 1


{-| The round of the loser bracket, counted from 1 for its first round.
-}
loserRoundNumber : Layout -> Int -> Int
loserRoundNumber { k } round =
    2 * k + round


layout : Game -> Maybe Layout
layout game =
    Dict.keys game.rounds
        |> List.filter DE.isWinnerRound
        |> List.maximum
        |> Maybe.map
            (\k ->
                let
                    winnerHeight =
                        power k

                    loserTop =
                        winnerHeight + 2

                    winnerFinal =
                        1 + power (k - 1)

                    loserFinal =
                        loserTop + power (k - 2)
                in
                { k = k
                , loserTop = loserTop
                , rows =
                    if k > 1 then
                        loserTop + winnerHeight // 2

                    else
                        winnerHeight + 1
                , finalColumn =
                    if k > 1 then
                        loserColumn (2 * (k - 1)) + 1

                    else
                        1
                , finalCenter =
                    if k > 1 then
                        (winnerFinal + loserFinal) // 2

                    else
                        winnerFinal
                }
            )


{-| Round 1 is left of the loser bracket round 1, every later round left of the
loser bracket round that its losers drop into.
-}
winnerColumn : Int -> Int
winnerColumn t =
    max 0 (2 * t - 3)


loserColumn : Int -> Int
loserColumn u =
    u


position : Layout -> MatchKey -> Position
position l ( round, index ) =
    if DE.isWinnerRound round then
        let
            t =
                winnerRoundNumber l round
        in
        { column = winnerColumn t
        , center = 1 + (2 * index + 1) * power (t - 1)
        }

    else if DE.isLoserRound round then
        let
            u =
                loserRoundNumber l round
        in
        { column = loserColumn u
        , center = l.loserTop + (2 * index + 1) * power ((u + 1) // 2 - 1)
        }

    else
        { column = l.finalColumn + index
        , center = l.finalCenter
        }


{-| The number of the match within its round. Rounds can have gaps in their
keys, as left out matches keep their place.
-}
matchNumber : Game -> MatchKey -> Int
matchNumber game ( round, index ) =
    Dict.get round game.rounds
        |> Maybe.andThen (Dict.keys >> List.Extra.elemIndex index)
        |> Maybe.withDefault index
        |> (+) 1


roundSize : Game -> Int -> Int
roundSize game round =
    Dict.get round game.rounds
        |> Maybe.map Dict.size
        |> Maybe.withDefault 0


{-| A short name like "WB R2 M1" or "LB R3". The match number is left out in
rounds with one match.
-}
matchName : Layout -> Game -> MatchKey -> String
matchName l game (( round, _ ) as key) =
    let
        number =
            if roundSize game round > 1 then
                " M" ++ String.fromInt (matchNumber game key)

            else
                ""
    in
    if DE.isWinnerRound round then
        "WB R" ++ String.fromInt (winnerRoundNumber l round) ++ number

    else if DE.isLoserRound round then
        "LB R" ++ String.fromInt (loserRoundNumber l round) ++ number

    else
        "Grand final"


{-| Where the player of an empty slot comes from, e.g. "Loser R1 M3". The slots
of the reset match have none: they are only filled when it is needed.
-}
source : Layout -> Game -> PlayerSlot -> Maybe String
source l game slot =
    if Tuple.first slot == Tuple.first DE.resetMatch && Tuple.second slot >= 2 then
        Nothing

    else
        Knockout.keyedMatches game
            |> List.Extra.findMap
                (\( key, match ) ->
                    if match.winner == Just slot then
                        Just <| "Winner " ++ matchName l game key

                    else if match.loser == Just slot then
                        Just <| "Loser " ++ matchName l game key

                    else
                        Nothing
                )


title : Layout -> Game -> MatchKey -> List String
title l game (( round, _ ) as key) =
    if DE.isWinnerRound round then
        [ "WB Round " ++ String.fromInt (winnerRoundNumber l round)
        , "Match " ++ String.fromInt (matchNumber game key)
        ]

    else if DE.isLoserRound round then
        [ "LB Round " ++ String.fromInt (loserRoundNumber l round)
        , "Match " ++ String.fromInt (matchNumber game key)
        ]

    else if key == DE.grandFinal then
        [ "Grand final" ]

    else
        [ "Reset" ]


type LineKind
    = WinnerLine
    | DropLine
    | ResetLine Bool


{-| `leave` is how far below (+) or above (-) the middle of its match the line
leaves the box, in em.
-}
type alias Line =
    { from : MatchKey
    , leave : Float
    , to : PlayerSlot
    , kind : LineKind
    }


{-| The lines inside both brackets and into the grand final. A line from the
winner bracket to the loser bracket only for the highlighted player. Then the
winner line leaves at the winner and the drop line at the loser, so that they
don't start at the same place.
-}
lines : Game -> Path -> List ( MatchKey, Match ) -> List Line
lines game path matches =
    let
        resetShown =
            List.any (\( key, _ ) -> key == DE.resetMatch) matches
    in
    matches
        |> List.concatMap
            (\( key, match ) ->
                if key == DE.grandFinal then
                    if resetShown then
                        [ { from = key, leave = 0, to = ( 0, 2 ), kind = ResetLine (DE.resetNeeded game) } ]

                    else
                        []

                else if key == DE.resetMatch then
                    []

                else
                    let
                        drop : Maybe PlayerSlot
                        drop =
                            match.loser
                                |> Maybe.andThen
                                    (\to ->
                                        if Set.member to path.lines && DE.isWinnerRound (Tuple.first key) then
                                            Just to

                                        else
                                            Nothing
                                    )

                        winnerSide : Float
                        winnerSide =
                            if
                                (match.winner |> Maybe.andThen (\slot -> Dict.get slot game.slots))
                                    == Dict.get match.player2 game.slots
                            then
                                1

                            else
                                -1

                        leave : Float
                        leave =
                            if drop == Nothing then
                                0

                            else
                                winnerSide
                    in
                    List.filterMap identity
                        [ match.winner
                            |> Maybe.map (\to -> { from = key, leave = leave, to = to, kind = WinnerLine })
                        , drop
                            |> Maybe.map (\to -> { from = key, leave = -leave, to = to, kind = DropLine })
                        ]
            )


{-| The match of the slot and how far its player is above (-) or below (+) the
middle of the box, in em. The reset match line ends in the middle.
-}
slotPosition : Layout -> PlayerSlot -> Maybe ( Position, Float )
slotPosition l (( round, index ) as slot) =
    if round == Knockout.placementRound then
        Nothing

    else
        Just
            ( position l ( round, index // 2 )
            , if slot == ( 0, 2 ) then
                0

              else if modBy 2 index == 0 then
                -1

              else
                1
            )


{-| A height in the line box: a number of rows from its top plus an offset in
em.
-}
type alias Height =
    ( Int, Float )


{-| A line leaves the right side of its match, turns once
vertically and enters the left side of the target box at the row of the slot.
It is drawn in a box from the column after its match to the target column,
with a row of space above and below, so that its parts are placed in percent
of the rows and em. A drop line turns right after its match, so that it
doesn't cross the loser bracket rounds it passes.
-}
viewLine : Layout -> Game -> Path -> Line -> Maybe (Html msg)
viewLine l game path line =
    slotPosition l line.to
        |> Maybe.map
            (\( target, offset ) ->
                let
                    from =
                        position l line.from

                    top =
                        min from.center target.center - 1

                    rows =
                        max from.center target.center + 1 - top

                    css : Height -> String
                    css ( row, em ) =
                        "calc("
                            ++ String.fromFloat (100 * toFloat (row - top) / toFloat rows)
                            ++ "% + "
                            ++ String.fromFloat em
                            ++ "em)"

                    start : Height
                    start =
                        ( from.center, line.leave - 0.2 )

                    end : Height
                    end =
                        ( target.center, offset - 0.2 )

                    ( upper, lower ) =
                        if compare start end == GT then
                            ( end, start )

                        else
                            ( start, end )

                    turn : String
                    turn =
                        if line.kind == DropLine then
                            "1.2em"

                        else
                            "50%"

                    player =
                        Dict.get line.to game.slots
                            |> Maybe.andThen (\id -> Dict.get id game.players)
                in
                div
                    (HA.classList
                        [ ( "double-elimination-line", True )
                        , ( "taken", player /= Nothing )
                        , ( "killed", Maybe.map (.alive >> not) player == Just True )
                        , ( "highlight", isHighlighted path line )
                        , ( "drop", line.kind == DropLine )
                        , ( "dashed", line.kind == DropLine || line.kind == ResetLine False )
                        ]
                        :: gridArea (2 * from.column + 2) (2 * target.column + 1) (top + 1) (top + rows + 1)
                    )
                    [ div
                        [ class "double-elimination-line-across"
                        , HA.style "left" "0"
                        , HA.style "width" turn
                        , HA.style "top" <| css start
                        ]
                        []
                    , if start == end then
                        text ""

                      else
                        div
                            [ class "double-elimination-line-down"
                            , HA.style "left" <| "calc(" ++ turn ++ " - 0.2em)"
                            , HA.style "top" <| css upper
                            , HA.style "height" <| "calc(" ++ css lower ++ " - " ++ css upper ++ " + 0.4em)"
                            ]
                            []
                    , div
                        [ class "double-elimination-line-across"
                        , HA.style "left" turn
                        , HA.style "right" "0"
                        , HA.style "top" <| css end
                        ]
                        []
                    ]
            )


{-| The line to the reset match stands for both players of the grand final.
-}
isHighlighted : Path -> Line -> Bool
isHighlighted path line =
    case line.kind of
        ResetLine _ ->
            Set.member ( 0, 2 ) path.lines || Set.member ( 0, 3 ) path.lines

        _ ->
            Set.member line.to path.lines


gridArea : Int -> Int -> Int -> Int -> List (Html.Attribute msg)
gridArea colStart colEnd rowStart rowEnd =
    [ HA.style "grid-column" <| String.fromInt colStart ++ " / " ++ String.fromInt colEnd
    , HA.style "grid-row" <| String.fromInt rowStart ++ " / " ++ String.fromInt rowEnd
    ]


{-| Only editors see the buttons to undo a result. The matches keep their
places and numbers when the finished ones are hidden. The reset match has a
dashed outline until it is needed and is hidden once the tournament was
decided without it.
-}
view : { names : Dict PlayerId String, canEdit : Bool, hideFinished : Bool, highlight : Maybe PlayerId } -> State -> Html Msg
view config { settings, game } =
    case layout game of
        Nothing ->
            text ""

        Just l ->
            let
                path : Path
                path =
                    config.highlight
                        |> Maybe.map (\id -> Knockout.playerPath id game)
                        |> Maybe.withDefault (Path Set.empty Set.empty Set.empty)

                shown : Game
                shown =
                    if config.hideFinished then
                        Knockout.hideFinishedMatches game

                    else
                        game

                matches : List ( MatchKey, Match )
                matches =
                    Knockout.keyedMatches shown
                        |> List.filter (\( key, _ ) -> key /= DE.resetMatch || not (DE.isResetSkipped game))

                lastColumn : Int
                lastColumn =
                    if Knockout.matchAt DE.resetMatch game /= Nothing && not (DE.isResetSkipped game) then
                        l.finalColumn + 1

                    else
                        l.finalColumn

                viewSection : ( MatchKey, Match ) -> Html Msg
                viewSection ( key, match ) =
                    let
                        pos =
                            position l key

                        bestOf : BestOf
                        bestOf =
                            DE.matchLength settings key
                    in
                    div
                        (HA.classList
                            [ ( "game-section-wrapper", True )
                            , ( "inactive", key == DE.resetMatch && not (DE.resetNeeded game) )
                            ]
                            :: gridArea (2 * pos.column + 1) (2 * pos.column + 2) pos.center (pos.center + 2)
                        )
                        [ Knockout.viewMatch
                            { names = config.names
                            , canEdit = config.canEdit
                            , bestOf = bestOf
                            , title = title l game key
                            , playable = DE.playable key game
                            , highlight = path.slots
                            , source = source l game
                            }
                            game
                            key
                            match
                        ]

                label : Int -> Int -> String -> Html msg
                label column row name =
                    div (class "double-elimination-label" :: gridArea (2 * column + 1) -1 row (row + 1))
                        [ text name ]

                -- the loser bracket can start with an empty round
                firstLoserColumn : Int
                firstLoserColumn =
                    Knockout.keyedMatches game
                        |> List.filter (Tuple.first >> Tuple.first >> DE.isLoserRound)
                        |> List.map (Tuple.first >> position l >> .column)
                        |> List.minimum
                        |> Maybe.withDefault (loserColumn 1)
            in
            div
                [ class "game double-elimination"
                , Knockout.onMouse "pointerleave" <| Highlight LeaveBracket
                , Knockout.onTap <| Highlight TapEmpty
                , HA.style "grid-template-rows" <| "repeat(" ++ String.fromInt l.rows ++ ", 1fr)"
                , HA.style "grid-template-columns" <|
                    if lastColumn > 0 then
                        "repeat(" ++ String.fromInt lastColumn ++ ", 3fr minmax(5em, 1fr)) 3fr"

                    else
                        "3fr"
                ]
            <|
                (label 0 1 "Winner bracket"
                    :: List.filterMap (viewLine l game path) (lines game path matches)
                )
                    ++ (if l.k > 1 then
                            [ label firstLoserColumn l.loserTop "Loser bracket" ]

                        else
                            []
                       )
                    ++ List.map viewSection matches
