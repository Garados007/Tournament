module Bracket.Ranking exposing (Criterion, groupWhile, placements, rank)

{-| Ranks the players of a table by a list of criteria.
-}

import Placement exposing (Placement)
import PlayerId exposing (PlayerId)


{-| Gets the players that are still tied and returns a value for one of them.
Higher values rank higher.
-}
type alias Criterion a =
    List a -> a -> Int


{-| Splits every group of tied players by the criterion, best first.
-}
refine : Criterion a -> List (List a) -> List (List a)
refine criterion =
    List.concatMap
        (\group ->
            case group of
                [ _ ] ->
                    [ group ]

                _ ->
                    group
                        |> List.map (\item -> ( criterion group item, item ))
                        |> List.sortBy (Tuple.first >> negate)
                        |> groupWhile (\( a, _ ) ( b, _ ) -> a == b)
                        |> List.map (List.map Tuple.second)
        )


{-| The items in rank order with their places. Tied items share a place, and
the next place is skipped for each of them. Tied items keep their order.
-}
rank : List (Criterion a) -> List a -> List ( Int, a )
rank criteria items =
    List.foldl refine [ items ] criteria
        |> List.filter (not << List.isEmpty)
        |> List.foldl
            (\group ( list, place ) ->
                ( list ++ List.map (Tuple.pair place) group
                , place + List.length group
                )
            )
            ( [], 1 )
        |> Tuple.first


groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile same list =
    List.foldr
        (\item groups ->
            case groups of
                (first :: rest) :: others ->
                    if same item first then
                        (item :: first :: rest) :: others

                    else
                        [ item ] :: groups

                _ ->
                    [ item ] :: groups
        )
        []
        list


{-| The places 1 to 3 of players in rank order. Players without a place are
skipped.
-}
placements : List ( Maybe Int, PlayerId ) -> List Placement
placements ranked =
    ranked
        |> List.filterMap
            (\( place, id ) ->
                place
                    |> Maybe.andThen
                        (\p ->
                            if p <= 3 then
                                Just ( p, id )

                            else
                                Nothing
                        )
            )
        |> groupWhile (\( a, _ ) ( b, _ ) -> a == b)
        |> List.filterMap
            (\group ->
                case group of
                    ( place, _ ) :: _ ->
                        Just { place = place, players = List.map Tuple.second group }

                    [] ->
                        Nothing
            )
