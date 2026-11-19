module Standings exposing
    ( Row
    , Standings
    , Status(..)
    , csv
    , order
    , played
    , podium
    , row
    )

{-| The ranking of all players of a tournament, for every mode.
-}

import Bracket.Ranking as Ranking
import Csv
import Dict exposing (Dict)
import PlayerId exposing (PlayerId)


{-| `Placed` players have a place. `Out` players are out without a place, e.g.
the semi-final losers of a knockout without a 3rd place.
-}
type Status
    = Placed
    | StillIn
    | Out
    | Removed


{-| `extra` has the values of the mode-specific `columns`.
-}
type alias Row =
    { id : PlayerId
    , place : Maybe Int
    , status : Status
    , won : Int
    , drawn : Int
    , lost : Int
    , extra : List String
    }


{-| `draws` tells whether the mode has draws. The places of a `liveRanking`
can change until the tournament is `finished`, the others are final once given.
-}
type alias Standings =
    { rows : List Row
    , finished : Bool
    , liveRanking : Bool
    , draws : Bool
    , columns : List String
    }


{-| A row whose status follows from the place. A removed player only keeps a
place that was decided before the removal.
-}
row : { id : PlayerId, place : Maybe Int, alive : Bool, removed : Bool } -> Row
row { id, place, alive, removed } =
    { id = id
    , place = place
    , status =
        if removed then
            Removed

        else if place /= Nothing then
            Placed

        else if alive then
            StillIn

        else
            Out
    , won = 0
    , drawn = 0
    , lost = 0
    , extra = []
    }


played : Row -> Int
played r =
    r.won + r.drawn + r.lost


{-| Players with a place by place, then the others by `progress` (lower got
further, players still in first), removed players without a place last. Ties
keep their order.
-}
order : (PlayerId -> Int) -> List Row -> List Row
order progress =
    List.sortBy
        (\r ->
            case ( r.place, r.status ) of
                ( Just place, _ ) ->
                    ( 0, place, 0 )

                ( Nothing, Placed ) ->
                    ( 0, 0, 0 )

                ( Nothing, StillIn ) ->
                    ( 1, progress r.id, 0 )

                ( Nothing, Out ) ->
                    ( 1, progress r.id, 1 )

                ( Nothing, Removed ) ->
                    ( 2, progress r.id, 0 )
        )


{-| The decided places 1 to 3 with their players. Places nobody has are left
out. A live ranking has none until the end.
-}
podium : Standings -> List ( Int, List PlayerId )
podium standings =
    (if standings.liveRanking && not standings.finished then
        []

     else
        standings.rows
    )
        |> List.filterMap
            (\r ->
                r.place
                    |> Maybe.andThen
                        (\place ->
                            if place <= 3 then
                                Just ( place, r.id )

                            else
                                Nothing
                        )
            )
        |> List.sortBy Tuple.first
        |> Ranking.groupWhile (\( a, _ ) ( b, _ ) -> a == b)
        |> List.filterMap
            (\group ->
                List.head group
                    |> Maybe.map (\( place, _ ) -> ( place, List.map Tuple.second group ))
            )


{-| The table with one row per player, as `Csv.encode` writes it.
-}
csv : Dict PlayerId String -> Standings -> String
csv names standings =
    let
        draws : List a -> List a
        draws values =
            if standings.draws then
                values

            else
                []
    in
    Csv.encode <|
        (([ "Place", "Name", "Played", "W" ] ++ draws [ "D" ] ++ [ "L" ])
            ++ standings.columns
            ++ [ "Note" ]
        )
            :: List.map
                (\r ->
                    [ Maybe.withDefault "" <| Maybe.map String.fromInt r.place
                    , Maybe.withDefault "" <| Dict.get r.id names
                    , String.fromInt <| played r
                    , String.fromInt r.won
                    ]
                        ++ draws [ String.fromInt r.drawn ]
                        ++ [ String.fromInt r.lost ]
                        ++ r.extra
                        ++ [ if r.status == Removed then
                                "removed"

                             else
                                ""
                           ]
                )
                standings.rows
