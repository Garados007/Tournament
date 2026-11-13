module Bracket.Knockout.Placement exposing (apply, placements)

{-| The placement slots are round -1. Index 0 is the first place, index 1 the
second. The third place has index 2, and with `Shared` also index 3.
-}

import Bracket.Knockout exposing (..)
import Dict exposing (Dict)
import Placement exposing (Placement)
import PlayerId exposing (PlayerId)


thirdPlaceMatch : Match
thirdPlaceMatch =
    { player1 = ( 0, 2 )
    , player2 = ( 0, 3 )
    , winner = Just ( -1, 2 )
    , loser = Nothing
    }


{-| Sets up the final, the semi-finals and the third-place match. Run it again
whenever the bracket grows: a bracket that reaches the semi-finals only later
gets its third place then.
-}
apply : Settings -> Game -> Game
apply settings game =
    let
        hasSemiFinals : Bool
        hasSemiFinals =
            Dict.get 1 game.rounds
                |> Maybe.map (\round -> Dict.size round == 2)
                |> Maybe.withDefault False

        semiFinalLoser : Int -> Maybe PlayerSlot
        semiFinalLoser index =
            if hasSemiFinals then
                case settings.thirdPlace of
                    Playoff ->
                        Just ( 0, 2 + index )

                    Shared ->
                        Just ( -1, 2 + index )

                    NoThirdPlace ->
                        Nothing

            else
                Nothing

        final : Round -> Round
        final =
            Dict.update 0 (Maybe.map <| \match -> { match | winner = Just ( -1, 0 ), loser = Just ( -1, 1 ) })
                >> (if hasSemiFinals && settings.thirdPlace == Playoff then
                        Dict.insert 1 thirdPlaceMatch

                    else
                        Dict.remove 1
                   )

        semiFinals : Round -> Round
        semiFinals =
            Dict.map (\index match -> { match | loser = semiFinalLoser index })
    in
    { game
        | rounds =
            game.rounds
                |> Dict.update 0 (Maybe.map final)
                |> Dict.update 1 (Maybe.map semiFinals)
    }
        |> moveMissingLosers
        |> resolveRemoved


{-| Losers of decided matches that got a loser slot only afterwards move there
and are back in the game.
-}
moveMissingLosers : Game -> Game
moveMissingLosers game =
    let
        missingLoser : Match -> Maybe ( PlayerSlot, PlayerId )
        missingLoser match =
            case ( match.winner, match.loser, opponents match game ) of
                ( Just winner, Just target, Just ( id1, id2 ) ) ->
                    case Dict.get winner game.slots of
                        Nothing ->
                            Nothing

                        Just winnerId ->
                            if Dict.member target game.slots then
                                Nothing

                            else if winnerId == id1 then
                                Just ( target, id2 )

                            else
                                Just ( target, id1 )

                _ ->
                    Nothing

        move : ( PlayerSlot, PlayerId ) -> Game -> Game
        move ( target, id ) g =
            if Tuple.first target < 0 && isRemoved id g then
                g

            else
                { g
                    | slots = Dict.insert target id g.slots
                    , players =
                        Dict.update id
                            (Maybe.map <| \player -> { player | alive = not player.removed })
                            g.players
                }
    in
    allMatches game
        |> List.filterMap missingLoser
        |> List.foldl move game


placeOf : Int -> Int
placeOf index =
    min 3 (index + 1)


{-| The places that some match leads to.
-}
placements : Game -> List Placement
placements game =
    let
        indices : List Int
        indices =
            allMatches game
                |> List.concatMap (\match -> List.filterMap identity [ match.winner, match.loser ])
                |> List.filter (\( round, _ ) -> round == -1)
                |> List.map Tuple.second
                |> List.sort

        players : Dict Int (List PlayerId)
        players =
            List.foldr
                (\index ->
                    case Dict.get ( -1, index ) game.slots of
                        Just id ->
                            Dict.update (placeOf index) (Maybe.withDefault [] >> (::) id >> Just)

                        Nothing ->
                            identity
                )
                Dict.empty
                indices
    in
    indices
        |> List.map placeOf
        |> List.foldr
            (\place list ->
                if List.head list == Just place then
                    list

                else
                    place :: list
            )
            []
        |> List.map
            (\place ->
                { place = place
                , players = Maybe.withDefault [] <| Dict.get place players
                }
            )
