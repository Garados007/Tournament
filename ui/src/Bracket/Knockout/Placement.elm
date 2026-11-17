module Bracket.Knockout.Placement exposing (addPlayer, allPlacements, apply, placementBracket, placements, toStandings)

{-| The placement slots are round -1. Index 0 is the first place, index 1 the
second. The third place has index 2, and with `Shared` also index 3. With
`PlayAll` every index is the place minus one.
-}

import Bracket.Knockout exposing (..)
import Dict exposing (Dict)
import List.Extra
import Placement exposing (Placement)
import PlayerId exposing (PlayerId)
import Standings exposing (Row, Standings, Status(..))


thirdPlaceMatch : Match
thirdPlaceMatch =
    { player1 = ( 0, 2 )
    , player2 = ( 0, 3 )
    , winner = Just ( -1, 2 )
    , loser = Nothing
    , games = []
    }


{-| Sets up the final and the places below. Run it again whenever the bracket
grows: a bracket that reaches the semi-finals only later gets its third place
then, and the placement brackets of `PlayAll` grow with the main bracket.
-}
apply : Settings -> Game -> Game
apply settings game =
    if settings.places == PlayAll then
        applyAll game

    else
        applyThirdPlace settings game


applyThirdPlace : Settings -> Game -> Game
applyThirdPlace settings game =
    let
        hasSemiFinals : Bool
        hasSemiFinals =
            Dict.get 1 game.rounds
                |> Maybe.map (\round -> Dict.size round == 2)
                |> Maybe.withDefault False

        semiFinalLoser : Int -> Maybe PlayerSlot
        semiFinalLoser index =
            if hasSemiFinals then
                case settings.places of
                    Playoff ->
                        Just ( 0, 2 + index )

                    Shared ->
                        Just ( -1, 2 + index )

                    _ ->
                        Nothing

            else
                Nothing

        final : Round -> Round
        final =
            Dict.update 0 (Maybe.map <| \match -> { match | winner = Just ( -1, 0 ), loser = Just ( -1, 1 ) })
                >> (if hasSemiFinals && settings.places == Playoff then
                        Dict.update 1 (Maybe.withDefault thirdPlaceMatch >> Just)

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


placeSlot : Int -> PlayerSlot
placeSlot place =
    ( placementRound, place - 1 )


{-| Links the winner and the losers of a bracket to the places they can still
reach. The winner of the bracket gets the place `first`, the loser of its final
the next one. The losers of every earlier round play a placement bracket for
the places after those of the later rounds.

Returns the linked matches and the matches of the placement brackets.

-}
linkLosers : Int -> List ( MatchKey, Match ) -> ( List ( MatchKey, Match ), List ( MatchKey, Match ) )
linkLosers first matches =
    let
        -- the keys of every round, in the order of the matches
        rounds : Dict Int (List MatchKey)
        rounds =
            List.foldr
                (\( ( round, _ ) as key, _ ) -> Dict.update round (Maybe.withDefault [] >> (::) key >> Just))
                Dict.empty
                matches

        firstPlaceOf : Int -> Int
        firstPlaceOf round =
            Dict.filter (\r _ -> r < round) rounds
                |> Dict.values
                |> List.map List.length
                |> List.sum
                |> (+) (first + 1)

        brackets : Dict Int ( List PlayerSlot, List ( MatchKey, Match ) )
        brackets =
            Dict.map
                (\round keys ->
                    if round == 0 then
                        ( [ placeSlot (first + 1) ], [] )

                    else
                        placementBracket (firstPlaceOf round) (List.length keys)
                )
                rounds

        loserSlot : MatchKey -> Maybe PlayerSlot
        loserSlot (( round, _ ) as key) =
            Maybe.map2 Tuple.pair
                (Dict.get round rounds |> Maybe.andThen (List.Extra.elemIndex key))
                (Dict.get round brackets)
                |> Maybe.andThen (\( index, ( inputs, _ ) ) -> List.Extra.getAt index inputs)
    in
    ( List.map
        (\( ( round, _ ) as key, match ) ->
            ( key
            , { match
                | winner =
                    if round == 0 then
                        Just (placeSlot first)

                    else
                        match.winner
                , loser = loserSlot key
              }
            )
        )
        matches
    , List.concatMap Tuple.second (Dict.values brackets)
    )


{-| The placement bracket for `count` players and the places from `first` on,
with the placement brackets that follow from it. It is a knockout bracket like
the main bracket, with a pre-round for a count that isn't a power of two. A
single player gets the place directly.

Returns the slots that the players enter, from top to bottom, and the matches.

-}
placementBracket : Int -> Int -> ( List PlayerSlot, List ( MatchKey, Match ) )
placementBracket first count =
    if count < 2 then
        ( List.repeat count (placeSlot first), [] )

    else
        let
            bracket : Game
            bracket =
                generateTournament count

            local : PlayerSlot -> PlayerSlot
            local ( round, index ) =
                if round >= 0 then
                    ( placementBracketRound first round, index )

                else
                    ( round, index )

            ( linked, following ) =
                linkLosers first (keyedMatches bracket)
        in
        ( initialSlots bracket
            |> List.sort
            |> List.map local
        , List.map
            (\( key, match ) ->
                ( local key
                , { match
                    | player1 = local match.player1
                    , player2 = local match.player2
                    , winner = Maybe.map local match.winner
                  }
                )
            )
            linked
            ++ following
        )


sameLinks : Match -> Match -> Bool
sameLinks a b =
    { a | games = [] } == { b | games = [] }


{-| Builds the placement brackets for the main bracket. Placement matches that
stay as they are keep their games. Slots that no match leads to any more are
cleared, and the losers move to their new slots.
-}
applyAll : Game -> Game
applyAll game =
    let
        ( linked, brackets ) =
            keyedMatches game
                |> List.filter (\( ( round, _ ), _ ) -> round >= 0)
                |> linkLosers 1

        keepGames : ( MatchKey, Match ) -> ( MatchKey, Match )
        keepGames ( key, match ) =
            case matchAt key game of
                Just old ->
                    if sameLinks old match then
                        ( key, old )

                    else
                        ( key, match )

                Nothing ->
                    ( key, match )
    in
    { game
        | rounds =
            List.foldl
                (\( ( round, index ), match ) -> Dict.update round (Maybe.withDefault Dict.empty >> Dict.insert index match >> Just))
                Dict.empty
                (linked ++ List.map keepGames brackets)
    }
        |> dropStaleSlots
        |> moveMissingLosers
        |> resolveRemoved


{-| Removes the players from the slots that they can't have reached. Only the
slots of the main round are no target of a match. A winner slot is taken by a
player of its match, a loser slot by the player that didn't take the winner
slot.
-}
dropStaleSlots : Game -> Game
dropStaleSlots game =
    let
        sources : Dict PlayerSlot Match
        sources =
            allMatches game
                |> List.concatMap
                    (\match ->
                        List.filterMap identity [ match.winner, match.loser ]
                            |> List.map (\slot -> ( slot, match ))
                    )
                |> Dict.fromList

        valid : PlayerSlot -> PlayerId -> Bool
        valid slot id =
            case ( Dict.get slot sources, opponents_ slot ) of
                ( Nothing, _ ) ->
                    Tuple.first slot >= 0

                ( Just match, Just ( id1, id2 ) ) ->
                    (id == id1 || id == id2)
                        && (match.winner == Just slot || winnerOf match == Just (other id1 id2 id))

                ( Just _, Nothing ) ->
                    False

        opponents_ : PlayerSlot -> Maybe ( PlayerId, PlayerId )
        opponents_ slot =
            Dict.get slot sources |> Maybe.andThen (\match -> opponents match game)

        winnerOf : Match -> Maybe PlayerId
        winnerOf match =
            match.winner |> Maybe.andThen (\slot -> Dict.get slot game.slots)

        other : PlayerId -> PlayerId -> PlayerId -> PlayerId
        other id1 id2 id =
            if id == id1 then
                id2

            else
                id1

        stale : Dict PlayerSlot PlayerId
        stale =
            Dict.filter (\slot id -> not <| valid slot id) game.slots
    in
    if Dict.isEmpty stale then
        game

    else
        dropStaleSlots { game | slots = Dict.diff game.slots stale }


{-| Registers the player and adds them to the bracket like
`Bracket.Knockout.addPlayer`. Their position must not change a placement
bracket that has results; then the player is only registered.
-}
addPlayer : Settings -> PlayerId -> Game -> Game
addPlayer settings id game =
    let
        grown : Game
        grown =
            Bracket.Knockout.addPlayer id game
                |> apply settings

        kept : ( MatchKey, Match ) -> Bool
        kept ( ( round, _ ) as key, match ) =
            round
                >= 0
                || not (hasResult match game)
                || (Maybe.map (sameLinks match) (matchAt key grown) == Just True)
    in
    if List.all kept (keyedMatches game) then
        grown

    else
        { game | players = Dict.insert id (Player id True False) game.players }


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
            if Tuple.first target == placementRound && isRemoved id g then
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


{-| The places that some match leads to. Only the first three for the banner.
-}
placements : Settings -> Game -> List Placement
placements settings game =
    if settings.places == PlayAll then
        allPlacements game
            |> List.filter (\placement -> placement.place <= 3)

    else
        placementsWith placeOf game


{-| Every place of a tournament with `PlayAll`.
-}
allPlacements : Game -> List Placement
allPlacements =
    placementsWith ((+) 1)


placementsWith : (Int -> Int) -> Game -> List Placement
placementsWith place game =
    let
        indices : List Int
        indices =
            allMatches game
                |> List.concatMap (\match -> List.filterMap identity [ match.winner, match.loser ])
                |> List.filter (\( round, _ ) -> round == placementRound)
                |> List.map Tuple.second
                |> List.sort

        players : Dict Int (List PlayerId)
        players =
            List.foldr
                (\index ->
                    case Dict.get ( placementRound, index ) game.slots of
                        Just id ->
                            Dict.update (place index) (Maybe.withDefault [] >> (::) id >> Just)

                        Nothing ->
                            identity
                )
                Dict.empty
                indices
    in
    indices
        |> List.map place
        |> List.foldr
            (\p list ->
                if List.head list == Just p then
                    list

                else
                    p :: list
            )
            []
        |> List.map
            (\p ->
                { place = p
                , players = Maybe.withDefault [] <| Dict.get p players
                }
            )


{-| Every player with a slot. The places from the placement slots come first.
With `Playoff` and `Shared` the losers of a main bracket round share the place
after all players who got further; the loser of the 3rd place match is 4th.
`PlayAll` and `NoThirdPlace` only give the places of the slots. A player who
was removed and lost by walkover gets no place.
-}
toStandings : Settings -> Game -> Standings
toStandings settings game =
    let
        allResults : List MatchResult
        allResults =
            results (matchLength settings) game

        isThirdPlaceMatch : MatchKey -> Bool
        isThirdPlaceMatch key =
            settings.places /= PlayAll && key == ( 0, 1 )

        mainLoss : Dict PlayerId MatchKey
        mainLoss =
            allResults
                |> List.filter (\result -> Tuple.first result.key >= 0 && not (isThirdPlaceMatch result.key))
                |> List.map (\result -> ( result.loser, result.key ))
                |> Dict.fromList

        slotPlace : Dict PlayerId Int
        slotPlace =
            (if settings.places == PlayAll then
                allPlacements game

             else
                placementsWith placeOf game
            )
                |> List.concatMap (\placement -> List.map (\id -> ( id, placement.place )) placement.players)
                |> Dict.fromList

        derivedPlace : PlayerId -> Maybe Int
        derivedPlace id =
            if settings.places == Playoff || settings.places == Shared then
                case List.Extra.find (\result -> result.loser == id) allResults of
                    Just result ->
                        if result.walkover && isRemoved id game then
                            Nothing

                        else if isThirdPlaceMatch result.key then
                            Just 4

                        else
                            Dict.get id mainLoss
                                |> Maybe.map (\( round, _ ) -> 2 ^ round + 1)

                    Nothing ->
                        Nothing

            else
                Nothing

        placeOfPlayer : Player -> Maybe Int
        placeOfPlayer player =
            case Dict.get player.id slotPlace of
                Just place ->
                    Just place

                Nothing ->
                    if player.alive then
                        Nothing

                    else
                        derivedPlace player.id

        -- the last main bracket round the player plays in, the 3rd place match counts as semi-final
        progress : PlayerId -> Int
        progress id =
            keyedMatches game
                |> List.filterMap
                    (\( ( round, _ ) as key, match ) ->
                        if round < 0 || not (List.member (Just id) [ Dict.get match.player1 game.slots, Dict.get match.player2 game.slots ]) then
                            Nothing

                        else if isThirdPlaceMatch key then
                            Just 1

                        else
                            Just round
                    )
                |> List.minimum
                |> Maybe.withDefault 1000

        withGames : Bool
        withGames =
            settings.bestOf > 1 || Maybe.withDefault 1 settings.finalBestOf > 1

        toRow : Player -> Row
        toRow player =
            let
                base : Row
                base =
                    Standings.row
                        { id = player.id
                        , place = placeOfPlayer player
                        , alive = player.alive
                        , removed = player.removed
                        }

                won : List MatchResult
                won =
                    List.filter (\result -> result.winner == player.id) allResults

                lost : List MatchResult
                lost =
                    List.filter (\result -> result.loser == player.id) allResults

                ( gamesWon, gamesLost ) =
                    List.map .games won
                        ++ List.map (.games >> (\( a, b ) -> ( b, a ))) lost
                        |> List.foldl (\( a, b ) ( sumA, sumB ) -> ( sumA + a, sumB + b )) ( 0, 0 )
            in
            { base
                | won = List.length won
                , lost = List.length lost
                , extra =
                    (Dict.get player.id mainLoss
                        |> Maybe.map (Tuple.first >> roundName game)
                        |> Maybe.withDefault ""
                    )
                        :: (if withGames then
                                [ String.fromInt gamesWon ++ " : " ++ String.fromInt gamesLost ]

                            else
                                []
                           )
            }

        rows : List Row
        rows =
            participants game
                |> List.filterMap (\id -> Dict.get id game.players)
                |> List.map toRow
                |> Standings.order progress
    in
    { rows = rows
    , finished = not (List.isEmpty rows) && List.all (.status >> (/=) StillIn) rows
    , draws = False
    , columns =
        "Out in"
            :: (if withGames then
                    [ "Games" ]

                else
                    []
               )
    }
