module Bracket.Scoring exposing
    ( Cell(..)
    , Outcome(..)
    , Scoring(..)
    , cellOutcome
    , decode
    , encode
    , flip
    , matchPoints
    , normalize
    )

{-| How the results of the modes with a table are entered and counted.
-}

import Codec
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Scoring
    = Points
    | WinnerOnly


{-| A match from the view of one player. `Forfeit True` is a win. A match
between two removed players is `Cancelled`.
-}
type Cell
    = Open
    | Played ( Int, Int )
    | Forfeit Bool
    | Cancelled


{-| The result of the player whose view the cell shows.
-}
type Outcome
    = Win
    | Draw
    | Loss


cellOutcome : Cell -> Maybe Outcome
cellOutcome c =
    case c of
        Open ->
            Nothing

        Played ( a, b ) ->
            Just <|
                case compare a b of
                    GT ->
                        Win

                    EQ ->
                        Draw

                    LT ->
                        Loss

        Forfeit True ->
            Just Win

        Forfeit False ->
            Just Loss

        Cancelled ->
            Nothing


{-| The same match from the view of the opponent.
-}
flip : Cell -> Cell
flip c =
    case c of
        Open ->
            Open

        Played ( a, b ) ->
            Played ( b, a )

        Forfeit won ->
            Forfeit <| not won

        Cancelled ->
            Cancelled


matchPoints : Outcome -> Int
matchPoints outcome =
    case outcome of
        Win ->
            3

        Draw ->
            1

        Loss ->
            0


{-| With `WinnerOnly` the scores are 1:0, 0:0 or 0:1.
-}
normalize : Scoring -> ( Int, Int ) -> ( Int, Int )
normalize scoring (( a, b ) as score) =
    case scoring of
        Points ->
            score

        WinnerOnly ->
            case compare a b of
                GT ->
                    ( 1, 0 )

                EQ ->
                    ( 0, 0 )

                LT ->
                    ( 0, 1 )


values : List ( String, Scoring )
values =
    [ ( "points", Points ), ( "winnerOnly", WinnerOnly ) ]


decode : Decoder Scoring
decode =
    Codec.decodeEnum "scoring" values


encode : Scoring -> Value
encode =
    Codec.encodeEnum values
