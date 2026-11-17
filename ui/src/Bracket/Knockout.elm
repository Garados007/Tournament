module Bracket.Knockout exposing (..)

import Bitwise
import Bracket.BestOf as BestOf exposing (BestOf, Games)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Json.Decode as JD exposing (Decoder)
import List.Extra
import PlayerId exposing (PlayerId)
import Set exposing (Set)


{-| A place a player can occupy: the round and the index within it. A player
occupies one more slot for every match they win.

Round 0 is the final, higher rounds come earlier. Round -1 holds the placement
slots, the index is the place minus one. Lower rounds belong to the placement
brackets (`placementBracketRound`).

-}
type alias PlayerSlot =
    ( Int, Int )


placementRound : Int
placementRound =
    -1


{-| The round of the placement bracket for the places from `first` on that
matches `round` of a knockout bracket: `-100 * first` for its final and lower
for the rounds before. The first place of a placement bracket is at least 3.
-}
placementBracketRound : Int -> Int -> Int
placementBracketRound first round =
    negate (100 * first + round)


{-| The first place of the placement bracket of a round and the round within the
bracket.
-}
placementBracketOf : Int -> Maybe ( Int, Int )
placementBracketOf round =
    if round < placementRound then
        Just ( negate round // 100, modBy 100 (negate round) )

    else
        Nothing


{-| A match between the players in `player1` and `player2`. The winner and the
loser move to the given slots of a later match. Which matches have a loser slot
depends on the `Places` setting.

The match is decided once its winner moved on, either by winning enough
`games` or by a walkover.

-}
type alias Match =
    { player1 : PlayerSlot
    , player2 : PlayerSlot
    , winner : Maybe PlayerSlot
    , loser : Maybe PlayerSlot
    , games : Games
    }


{-| The round of a match and its index within the round.
-}
type alias MatchKey =
    ( Int, Int )


{-| All matches of one round (like quarter-finals, semi-finals, etc.), keyed by
the index of the match in the round.
-}
type alias Round =
    Dict Int Match


{-| A removed player left the tournament. They are out, and their matches are
decided in favour of the opponent.
-}
type alias Player =
    { id : PlayerId
    , alive : Bool
    , removed : Bool
    }


type alias Game =
    { rounds : Dict Int Round
    , slots : Dict PlayerSlot PlayerId
    , players : Dict PlayerId Player
    }


{-| A running knockout tournament with the settings it was started with.
-}
type alias State =
    { settings : Settings
    , game : Game
    }


{-| Without `finalBestOf` the final is as long as the other matches.
-}
type alias Settings =
    { places : Places
    , bestOf : BestOf
    , finalBestOf : Maybe BestOf
    }


{-| Which places are awarded. `Playoff`, `Shared` and `NoThirdPlace` only differ
in the 3rd place. With `PlayAll` the losers of every round play on for the
places they can still reach.
-}
type Places
    = Playoff
    | Shared
    | NoThirdPlace
    | PlayAll


defaultSettings : Settings
defaultSettings =
    { places = Playoff
    , bestOf = 1
    , finalBestOf = Nothing
    }


{-| The final is the match `( 0, 0 )`.
-}
matchLength : Settings -> MatchKey -> BestOf
matchLength settings key =
    if key == ( 0, 0 ) then
        Maybe.withDefault settings.bestOf settings.finalBestOf

    else
        settings.bestOf


type Msg
    = WinGame Bool MatchKey
    | Undo MatchKey
    | Highlight HighlightMsg


{-| A mouse highlights the player it points at until it leaves the bracket. A
tap selects a player until the next tap on them or on empty space.
-}
type HighlightMsg
    = HoverPlayer PlayerId
    | LeaveBracket
    | TapPlayer PlayerId
    | TapEmpty


updateHighlight : HighlightMsg -> Maybe PlayerId -> Maybe PlayerId
updateHighlight msg highlight =
    case msg of
        HoverPlayer id ->
            Just id

        LeaveBracket ->
            Nothing

        TapPlayer id ->
            if highlight == Just id then
                Nothing

            else
                Just id

        TapEmpty ->
            Nothing


{-| The part of the bracket a player went through: the slots they took, the
matches they are in and the lines they moved along. A line is identified by
the slot it leads to.
-}
type alias Path =
    { slots : Set PlayerSlot
    , matches : Set MatchKey
    , lines : Set PlayerSlot
    }


playerPath : PlayerId -> Game -> Path
playerPath id game =
    let
        slots : Set PlayerSlot
        slots =
            game.slots
                |> Dict.filter (\_ slotId -> slotId == id)
                |> Dict.keys
                |> Set.fromList

        played : List ( MatchKey, Match )
        played =
            keyedMatches game
                |> List.filter
                    (\( _, match ) ->
                        Set.member match.player1 slots || Set.member match.player2 slots
                    )
    in
    { slots = slots
    , matches = Set.fromList <| List.map Tuple.first played
    , lines =
        played
            |> List.concatMap (\( _, match ) -> List.filterMap identity [ match.winner, match.loser ])
            |> List.filter (\slot -> Set.member slot slots)
            |> Set.fromList
    }


generateRound : Int -> Int -> Round
generateRound playerCount round =
    Dict.fromList <|
        List.map
            (\i ->
                Tuple.pair i
                    { player1 = ( round, i * 2 )
                    , player2 = ( round, i * 2 + 1 )
                    , winner =
                        if round > 0 then
                            Just ( round - 1, i )

                        else
                            Nothing
                    , loser = Nothing
                    , games = []
                    }
            )
        <|
            List.range 0 (playerCount // 2 - 1)


{-| The round number of the main round. Its slot count is the largest power of
two up to the player count; the players that don't fit play a pre-round first.
-1 for fewer than two players.
-}
mainRound : Int -> Int
mainRound playerCount =
    if playerCount > 1 then
        1 + mainRound (playerCount // 2)

    else
        -1


{-| The number of slots of the main round.
-}
mainRoundSize : Int -> Int
mainRoundSize playerCount =
    Bitwise.shiftLeftBy (mainRound playerCount + 1) 1


{-| The pre-round matches feed the top slots of the main round.
-}
generateTournament : Int -> Game
generateTournament playerCount =
    let
        size : Int
        size =
            mainRoundSize playerCount
    in
    if playerCount < 2 then
        Game Dict.empty Dict.empty Dict.empty

    else
        generateBracket (mainRound playerCount) <|
            List.map (\index -> index < playerCount - size) <|
                List.range 0 (size - 1)


{-| A bracket whose main round is `maxRound`. Every slot of the main round that
is `True` in `preRound` gets the winner of a pre-round match, the others get a
player directly. Pre-round match `i` feeds the main round slot `i`.

The initial slots are numbered from top to bottom.

-}
generateBracket : Int -> List Bool -> Game
generateBracket maxRound preRound =
    let
        hasPreRound : Bool
        hasPreRound =
            List.member True preRound

        ( inputs, preMatches ) =
            if hasPreRound then
                preRound
                    |> List.indexedMap Tuple.pair
                    |> List.foldl
                        (\( index, fed ) ( slots, matches, next ) ->
                            if fed then
                                ( ( maxRound, index ) :: slots
                                , ( index
                                  , { player1 = ( maxRound + 1, next )
                                    , player2 = ( maxRound + 1, next + 1 )
                                    , winner = Just ( maxRound, index )
                                    , loser = Nothing
                                    , games = []
                                    }
                                  )
                                    :: matches
                                , next + 2
                                )

                            else
                                ( ( maxRound + 1, next ) :: slots, matches, next + 1 )
                        )
                        ( [], [], 0 )
                    |> (\( slots, matches, _ ) -> ( Dict.fromList <| List.indexedMap Tuple.pair <| List.reverse slots, matches ))

            else
                ( Dict.empty, [] )

        input : PlayerSlot -> PlayerSlot
        input (( _, index ) as slot) =
            Dict.get index inputs
                |> Maybe.withDefault slot

        mainRoundMatches : Round
        mainRoundMatches =
            generateRound (List.length preRound) maxRound
                |> Dict.map
                    (\_ match ->
                        { match
                            | player1 = input match.player1
                            , player2 = input match.player2
                        }
                    )
    in
    Game
        (List.range 0 (maxRound - 1)
            |> List.map
                (\round ->
                    ( round
                    , generateRound (Bitwise.shiftLeftBy (round + 1) 1) round
                    )
                )
            |> (::) ( maxRound, mainRoundMatches )
            |> (if hasPreRound then
                    (::) ( maxRound + 1, Dict.fromList preMatches )

                else
                    identity
               )
            |> Dict.fromList
        )
        Dict.empty
        Dict.empty


initialSlots : Game -> List PlayerSlot
initialSlots game =
    let
        maxRound : Int
        maxRound =
            Dict.keys game.rounds |> List.maximum |> Maybe.withDefault 0
    in
    game.rounds
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concatMap
            (\match -> [ match.player1, match.player2 ])
        |> List.filter
            (\( round, _ ) -> round == maxRound)


setInitialPlayers : List PlayerId -> Game -> Game
setInitialPlayers list game =
    placePlayers (initialSlots game) list game


{-| Registers the players and puts them into the slots in list order.
-}
placePlayers : List PlayerSlot -> List PlayerId -> Game -> Game
placePlayers slots list game =
    { game
        | players =
            List.map (\id -> ( id, Player id True False )) list
                |> Dict.fromList
        , slots =
            list
                |> List.Extra.zip slots
                |> Dict.fromList
    }


addPosition : Game -> Maybe Game
addPosition game =
    let
        maxKey : Dict Int value -> Int
        maxKey =
            Dict.keys >> List.maximum >> Maybe.withDefault 0

        maxRound =
            maxKey game.rounds

        slotsInRound : Int -> Int
        slotsInRound n =
            Bitwise.shiftLeftBy n 1

        allPositions : List ( Int, Int )
        allPositions =
            List.concat
                [ List.map (Tuple.pair maxRound) <|
                    List.range 0 <|
                        slotsInRound maxRound
                            - 1
                , List.map (Tuple.pair <| maxRound + 1) <|
                    List.range 0 <|
                        slotsInRound (maxRound + 1)
                            - 1
                ]

        follower : ( Int, Int ) -> ( Int, Int )
        follower ( round, index ) =
            ( round - 1, index // 2 )

        posExists : ( Int, Int ) -> Bool
        posExists ( round, index ) =
            Dict.get round game.rounds
                |> Maybe.map (Dict.member index)
                |> Maybe.withDefault False

        posLocked : ( Int, Int ) -> Bool
        posLocked ( round, index ) =
            Dict.get round game.rounds
                |> Maybe.andThen (Dict.get index)
                |> Maybe.map (\match -> hasResult match game)
                |> Maybe.withDefault False

        availablePos : List ( Int, Int )
        availablePos =
            allPositions
                |> List.filter
                    (\pos ->
                        List.all identity
                            [ not <| posExists pos
                            , posExists <| follower pos
                            , not <| posLocked <| follower pos
                            ]
                    )

        getMatch : ( Int, Int ) -> Maybe Match
        getMatch ( round, index ) =
            Dict.get round game.rounds
                |> Maybe.andThen (Dict.get index)

        posToMatch : ( Int, Int ) -> ( PlayerSlot, Match )
        posToMatch ( round, index ) =
            getMatch (follower ( round, index ))
                |> Maybe.map
                    (\match ->
                        if modBy 2 index == 0 then
                            match.player1

                        else
                            match.player2
                    )
                -- unreachable: availablePos only contains positions whose follower exists
                |> Maybe.withDefault ( round, 0 )
                |> (\slot ->
                        Tuple.pair slot
                            { player1 = ( round, Tuple.second slot )
                            , player2 = ( round, Tuple.second slot + 1 )
                            , winner = Just <| ( round - 1, index )
                            , loser = Nothing
                            , games = []
                            }
                   )

        updateSlot : (PlayerSlot -> PlayerSlot) -> Game
        updateSlot mapper =
            { game
                | rounds =
                    Dict.map
                        (\_ ->
                            Dict.map
                                (\_ match ->
                                    { match
                                        | player1 = mapper match.player1
                                        , player2 = mapper match.player2
                                        , winner = Maybe.map mapper match.winner
                                        , loser = Maybe.map mapper match.loser
                                    }
                                )
                        )
                        game.rounds
                , slots =
                    Dict.toList game.slots
                        |> List.map (Tuple.mapFirst mapper)
                        |> Dict.fromList
            }

        insertSlot : PlayerSlot -> PlayerSlot -> PlayerSlot -> PlayerSlot
        insertSlot ( il, ii ) new ( vl, vi ) =
            if il == vl then
                if ii > vi then
                    ( vl, vi )

                else if ii < vi then
                    ( vl, vi + 1 )

                else
                    new

            else
                ( vl, vi )

        addSlotRound : PlayerSlot -> PlayerSlot
        addSlotRound ( round, index ) =
            if round == maxRound then
                ( round + 1, index )

            else
                ( round, index )
    in
    List.head availablePos
        |> Maybe.map
            (\(( round, index ) as pos) ->
                posToMatch pos
                    |> (\( slot, match ) ->
                            updateSlot
                                (if round == maxRound then
                                    insertSlot slot ( round - 1, index )

                                 else
                                    addSlotRound
                                        >> insertSlot
                                            ( round, Tuple.second slot )
                                            ( round - 1, index )
                                )
                                |> (\game_ ->
                                        { game_
                                            | rounds =
                                                Dict.update round
                                                    (Maybe.withDefault Dict.empty
                                                        >> Dict.insert index match
                                                        >> Just
                                                    )
                                                    game_.rounds
                                            , slots =
                                                case Dict.get ( round - 1, index ) game_.slots of
                                                    Just x ->
                                                        game_.slots
                                                            |> Dict.remove ( round - 1, index )
                                                            |> Dict.insert ( round, Tuple.second slot ) x

                                                    Nothing ->
                                                        game_.slots
                                        }
                                            |> Tuple.pair ( round, Tuple.second slot )
                                   )
                       )
            )
        |> Maybe.map
            (\( slot, game_ ) ->
                Dict.keys game_.players
                    |> List.filter (\x -> not <| isPlayerUsed x game_)
                    |> List.head
                    |> Maybe.map
                        (\id ->
                            { game_
                                | slots =
                                    Dict.insert
                                        (Tuple.mapSecond ((+) 1) slot)
                                        id
                                        game_.slots
                            }
                        )
                    |> Maybe.withDefault game_
            )


isPlayerUsed : PlayerId -> Game -> Bool
isPlayerUsed id game =
    Dict.values game.slots
        |> List.member id


{-| Creates a tournament with the players in list order.
-}
start : List PlayerId -> Game
start list =
    setInitialPlayers list <|
        generateTournament <|
            List.length list


{-| The seeds from top to bottom in a bracket with `size` slots, a power of two.
The seeds of every first round match add up to `size + 1`, so the best seeds
meet as late as possible: for 8 slots 1, 8, 4, 5, 2, 7, 3, 6.
-}
seedPositions : Int -> List Int
seedPositions size =
    if size <= 1 then
        [ 1 ]

    else
        seedPositions (size // 2)
            |> List.concatMap (\seed -> [ seed, size + 1 - seed ])


{-| Creates a tournament with the players in seed order, the best first. The
seeds take the `seedPositions` of a bracket that is filled up to the next power
of two with byes, so the best seeds skip the pre-round.
-}
startSeeded : List PlayerId -> Game
startSeeded list =
    let
        count : Int
        count =
            List.length list

        size : Int
        size =
            mainRoundSize count

        -- seeds of a match share the main round slot of the better seed
        opponent : Int -> Int
        opponent seed =
            2 * size + 1 - seed

        hasPreMatch : Int -> Bool
        hasPreMatch seed =
            opponent seed <= count

        mainSeeds : List Int
        mainSeeds =
            seedPositions size

        game : Game
        game =
            generateBracket (mainRound count) <|
                List.map hasPreMatch mainSeeds

        players : Dict Int PlayerId
        players =
            List.indexedMap (\index id -> ( index + 1, id )) list
                |> Dict.fromList
    in
    if count < 2 then
        start list

    else
        mainSeeds
            |> List.concatMap
                (\seed ->
                    if hasPreMatch seed then
                        [ seed, opponent seed ]

                    else
                        [ seed ]
                )
            |> List.filterMap (\seed -> Dict.get seed players)
            |> (\ordered -> placePlayers (List.sort <| initialSlots game) ordered game)


{-| Registers a new player and adds a match for it. Without a free position the
player is only registered.
-}
addPlayer : PlayerId -> Game -> Game
addPlayer id game =
    let
        withPlayer : Game
        withPlayer =
            { game
                | players =
                    Dict.insert id
                        (Player id True False)
                        game.players
            }
    in
    addPosition withPlayer
        |> Maybe.withDefault withPlayer
        |> resolveRemoved


{-| Marks the player as removed. Their opponents win by walkover.
-}
removePlayer : PlayerId -> Game -> Game
removePlayer id game =
    resolveRemoved
        { game
            | players =
                Dict.update id
                    (Maybe.map <| \player -> { player | alive = False, removed = True })
                    game.players
        }


{-| Whether the player is registered and out. Unknown players aren't out.
-}
isEliminated : PlayerId -> Game -> Bool
isEliminated id game =
    Dict.get id game.players
        |> Maybe.map (.alive >> not)
        |> Maybe.withDefault False


isRemoved : PlayerId -> Game -> Bool
isRemoved id game =
    Dict.get id game.players
        |> Maybe.map .removed
        |> Maybe.withDefault False


allMatches : Game -> List Match
allMatches game =
    Dict.values game.rounds
        |> List.concatMap Dict.values


keyedMatches : Game -> List ( MatchKey, Match )
keyedMatches game =
    Dict.toList game.rounds
        |> List.concatMap
            (\( round, matches ) ->
                Dict.toList matches
                    |> List.map (Tuple.mapFirst (Tuple.pair round))
            )


matchAt : MatchKey -> Game -> Maybe Match
matchAt ( round, index ) game =
    Dict.get round game.rounds
        |> Maybe.andThen (Dict.get index)


setGames : MatchKey -> Games -> Game -> Game
setGames ( round, index ) games game =
    { game
        | rounds =
            Dict.update round
                (Maybe.map <| Dict.update index (Maybe.map <| \match -> { match | games = games }))
                game.rounds
    }


{-| The match that has the slot as one of its players.
-}
matchWithInput : PlayerSlot -> Game -> Maybe Match
matchWithInput slot game =
    allMatches game
        |> List.Extra.find (\match -> match.player1 == slot || match.player2 == slot)


{-| A match without a winner slot is decided once one of its players is out.
-}
isDecided : Match -> Game -> Bool
isDecided match game =
    case match.winner of
        Just winner ->
            Dict.member winner game.slots

        Nothing ->
            [ match.player1, match.player2 ]
                |> List.filterMap (\slot -> Dict.get slot game.slots)
                |> List.any (\id -> isEliminated id game)


{-| Both players of the match, once both have arrived.
-}
opponents : Match -> Game -> Maybe ( PlayerId, PlayerId )
opponents match game =
    Maybe.map2 Tuple.pair
        (Dict.get match.player1 game.slots)
        (Dict.get match.player2 game.slots)


canSetWinner : Match -> Game -> Bool
canSetWinner match game =
    case opponents match game of
        Just ( id1, id2 ) ->
            not (isDecided match game)
                && not (isRemoved id1 game)
                && not (isRemoved id2 game)

        Nothing ->
            False


{-| A started match has a result, even if it isn't decided yet.
-}
hasResult : Match -> Game -> Bool
hasResult match game =
    isDecided match game || not (List.isEmpty match.games)


{-| Undo removes the last game. A decided result can be undone as long as the
matches its players moved on to have no result. Matches with a removed player
can't be undone, as that would bring the player back.
-}
canUndo : Match -> Game -> Bool
canUndo match game =
    case opponents match game of
        Just ( id1, id2 ) ->
            not (isRemoved id1 game)
                && not (isRemoved id2 game)
                && (if isDecided match game then
                        (match.winner /= Nothing)
                            && ([ match.winner, match.loser ]
                                    |> List.filterMap identity
                                    |> List.filterMap (\slot -> matchWithInput slot game)
                                    |> List.all (\next -> not <| hasResult next game)
                               )

                    else
                        not (List.isEmpty match.games)
                   )

        Nothing ->
            False


{-| Moves the winner on. The loser moves to the loser slot if there is one, but
a removed player never takes a placement slot. A loser who doesn't move is out.
-}
decide : Bool -> Match -> Game -> Game
decide isPlayer2 match game =
    let
        ( winnerSlot, loserSlot ) =
            if isPlayer2 then
                ( match.player2, match.player1 )

            else
                ( match.player1, match.player2 )

        loserId : Maybe PlayerId
        loserId =
            Dict.get loserSlot game.slots

        loserTarget : Maybe PlayerSlot
        loserTarget =
            case ( match.loser, loserId ) of
                ( Just ( round, index ), Just id ) ->
                    if round /= placementRound || not (isRemoved id game) then
                        Just ( round, index )

                    else
                        Nothing

                _ ->
                    Nothing

        insertIfGiven : Maybe comparable -> Maybe value -> Dict comparable value -> Dict comparable value
        insertIfGiven key value =
            case ( key, value ) of
                ( Just k, Just v ) ->
                    Dict.insert k v

                _ ->
                    identity
    in
    { game
        | slots =
            game.slots
                |> insertIfGiven match.winner (Dict.get winnerSlot game.slots)
                |> insertIfGiven loserTarget loserId
        , players =
            case ( loserTarget, loserId ) of
                ( Nothing, Just id ) ->
                    Dict.update id
                        (Maybe.map (\x -> { x | alive = False }))
                        game.players

                _ ->
                    game.players
    }


{-| Clears the slots the players moved on to and brings the loser back.
-}
undoDecision : Match -> Game -> Game
undoDecision match game =
    { game
        | slots =
            [ match.winner, match.loser ]
                |> List.filterMap identity
                |> List.foldl Dict.remove game.slots
        , players =
            [ match.player1, match.player2 ]
                |> List.filterMap (\slot -> Dict.get slot game.slots)
                |> List.foldl
                    (\id -> Dict.update id (Maybe.map (\x -> { x | alive = True })))
                    game.players
    }


{-| Decides every open match with a removed player. Two removed players only
meet in a double elimination or a placement bracket, as removed players still
move on to those as losers. Then player 1 moves on, so that the next opponent
wins by walkover, but nobody gets a placement slot that way.
-}
resolveRemoved : Game -> Game
resolveRemoved =
    resolveRemovedWhere (\_ _ -> True)


{-| Like `resolveRemoved`, but only for the matches that are `playable`.
-}
resolveRemovedWhere : (MatchKey -> Game -> Bool) -> Game -> Game
resolveRemovedWhere playable game =
    let
        walkover : ( MatchKey, Match ) -> Maybe ( Bool, Match )
        walkover ( key, match ) =
            case ( opponents match game, match.winner ) of
                ( Just ( id1, id2 ), Just ( winnerRound, _ ) ) ->
                    if isDecided match game || not (playable key game) then
                        Nothing

                    else
                        case ( isRemoved id1 game, isRemoved id2 game ) of
                            ( True, False ) ->
                                Just ( True, match )

                            ( False, True ) ->
                                Just ( False, match )

                            ( True, True ) ->
                                if winnerRound == placementRound then
                                    Nothing

                                else
                                    Just ( False, match )

                            ( False, False ) ->
                                Nothing

                _ ->
                    Nothing
    in
    case List.Extra.findMap walkover (keyedMatches game) of
        Just ( isPlayer2, match ) ->
            resolveRemovedWhere playable <| decide isPlayer2 match game

        Nothing ->
            game


{-| Brings a removed player back and undoes the walkovers they lost. `Nothing`
if a winner of such a walkover already has a result in their next match.
-}
restorePlayer : Settings -> PlayerId -> Game -> Maybe Game
restorePlayer settings =
    restorePlayerWith
        { matchLength = matchLength settings
        , playable = \_ _ -> True
        }


restorePlayerWith : Rules -> PlayerId -> Game -> Maybe Game
restorePlayerWith rules id game =
    if isRemoved id game then
        { game | players = Dict.update id (Maybe.map <| \player -> { player | removed = False }) game.players }
            |> undoWalkovers rules id
            |> Maybe.map
                (\restored ->
                    { restored
                        | players =
                            Dict.update id
                                (Maybe.map <| \player -> { player | alive = not <| hasLostOut id restored })
                                restored.players
                    }
                        |> resolveRemovedWhere rules.playable
                )

    else
        Nothing


{-| The player of a decided match who didn't win it.
-}
loserOf : Match -> Game -> Maybe PlayerId
loserOf match game =
    case ( Maybe.andThen (\slot -> Dict.get slot game.slots) match.winner, opponents match game ) of
        ( Just winner, Just ( id1, id2 ) ) ->
            if winner == id1 then
                Just id2

            else
                Just id1

        _ ->
            Nothing


{-| Whether the player lost a match and didn't move on to a loser slot.
-}
hasLostOut : PlayerId -> Game -> Bool
hasLostOut id game =
    allMatches game
        |> List.any
            (\match ->
                (loserOf match game == Just id)
                    && (Maybe.andThen (\slot -> Dict.get slot game.slots) match.loser /= Just id)
            )


{-| Undoes the walkovers the player lost, the last one first.
-}
undoWalkovers : Rules -> PlayerId -> Game -> Maybe Game
undoWalkovers rules id game =
    let
        walkovers : List Match
        walkovers =
            keyedMatches game
                |> List.filter
                    (\( key, match ) ->
                        (loserOf match game == Just id)
                            && (BestOf.winner (rules.matchLength key) match.games == Nothing)
                    )
                |> List.map Tuple.second

        isLast : Match -> Bool
        isLast match =
            [ match.winner, match.loser ]
                |> List.filterMap identity
                |> List.filterMap (\slot -> matchWithInput slot game)
                |> List.all (\next -> not <| hasResult next game)
    in
    case walkovers of
        [] ->
            Just game

        _ ->
            List.Extra.find isLast walkovers
                |> Maybe.andThen (\match -> undoWalkovers rules id <| undoDecision match game)


hideFinishedMatches : Game -> Game
hideFinishedMatches game =
    { game
        | rounds =
            game.rounds
                |> Dict.map
                    (\_ ->
                        Dict.filter <|
                            \_ match ->
                                match.winner
                                    |> Maybe.andThen (\x -> Dict.get x game.slots)
                                    |> (==) Nothing
                    )
                |> Dict.filter (\_ -> Dict.isEmpty >> not)
    }


viewPlayer : Dict PlayerId String -> Player -> Html msg
viewPlayer names player =
    div
        [ HA.classList
            [ ( "player", True )
            , ( "alive", player.alive )
            ]
        ]
        [ text <| Maybe.withDefault "" <| Dict.get player.id names
        ]


{-| The type of the pointer that caused the event. Older browsers send clicks
without one; they count as taps.
-}
pointerType : Decoder String
pointerType =
    JD.oneOf [ JD.field "pointerType" JD.string, JD.succeed "" ]


onMouse : String -> msg -> Html.Attribute msg
onMouse event msg =
    HE.on event <|
        JD.andThen
            (\pointer ->
                if pointer == "mouse" then
                    JD.succeed msg

                else
                    JD.fail "not a mouse"
            )
            pointerType


onTap : msg -> Html.Attribute msg
onTap msg =
    HE.on "click" <|
        JD.andThen
            (\pointer ->
                if pointer == "mouse" then
                    JD.fail "not a tap"

                else
                    JD.succeed msg
            )
            pointerType


{-| The click doesn't reach the bracket, so it doesn't clear the highlight.
-}
onButtonClick : msg -> Html.Attribute msg
onButtonClick msg =
    HE.stopPropagationOn "click" <| JD.succeed ( msg, True )


{-| A mouse click on a name highlights it like hovering does, so it doesn't
undo the highlight of the hover before.
-}
onPlayerClick : PlayerId -> Html.Attribute Msg
onPlayerClick id =
    HE.stopPropagationOn "click" <|
        JD.map
            (\pointer ->
                ( Highlight <|
                    if pointer == "mouse" then
                        HoverPlayer id

                    else
                        TapPlayer id
                , True
                )
            )
            pointerType


{-| How `viewMatch` shows a match. Only `playable` matches take games.
`source` describes where the player of an empty slot comes from.
-}
type alias MatchView =
    { names : Dict PlayerId String
    , canEdit : Bool
    , bestOf : BestOf
    , title : List String
    , playable : Bool
    , highlight : Set PlayerSlot
    , source : PlayerSlot -> Maybe String
    }


{-| A match box with its title, both players and the buttons for the results.
-}
viewMatch : MatchView -> Game -> MatchKey -> Match -> Html Msg
viewMatch config game key match =
    let
        viewMatchPlayer : Bool -> Html Msg
        viewMatchPlayer isPlayer2 =
            let
                slot : PlayerSlot
                slot =
                    if isPlayer2 then
                        match.player2

                    else
                        match.player1

                player : Maybe Player
                player =
                    Dict.get slot game.slots
                        |> Maybe.andThen (\id -> Dict.get id game.players)

                gamesWon : Int
                gamesWon =
                    BestOf.wins match.games
                        |> (if isPlayer2 then
                                Tuple.second

                            else
                                Tuple.first
                           )
            in
            div [ class "game-match-player" ] <|
                [ div
                    (HA.classList
                        [ ( "game-match-player-name", True )
                        , ( "highlight", Set.member slot config.highlight )
                        ]
                        :: (case player of
                                Just { id } ->
                                    [ onMouse "pointerenter" <| Highlight <| HoverPlayer id
                                    , onPlayerClick id
                                    ]

                                Nothing ->
                                    []
                           )
                    )
                  <|
                    case player of
                        Just p ->
                            [ viewPlayer config.names p ]

                        Nothing ->
                            case config.source slot of
                                Just source ->
                                    [ div [ class "game-match-player-source" ] [ text source ] ]

                                Nothing ->
                                    []
                ]
                    ++ (if config.bestOf > 1 then
                            [ div [ class "game-match-wins" ]
                                [ text <|
                                    if player == Nothing then
                                        ""

                                    else
                                        String.fromInt gamesWon
                                ]
                            ]

                        else
                            []
                       )
                    ++ [ div
                            [ HA.classList
                                [ ( "game-match-winner", True )
                                , ( "add-game", config.bestOf > 1 )
                                , ( "enabled", config.playable && canSetWinner match game )
                                ]
                            , HA.title <|
                                if config.bestOf > 1 then
                                    "Add a won game"

                                else
                                    "Select as winner"
                            , onButtonClick <| WinGame isPlayer2 key
                            ]
                            []
                       ]
    in
    div [ class "game-section" ]
        [ div [ class "game-section-title" ] <|
            List.map (\part -> div [] [ text part ]) config.title
                ++ (if config.bestOf > 1 then
                        [ div [] [ text <| "Best of " ++ String.fromInt config.bestOf ] ]

                    else
                        []
                   )
        , viewMatchPlayer False
        , viewMatchPlayer True
        , if config.canEdit && canUndo match game then
            div
                [ class "game-match-undo"
                , HA.title <|
                    if config.bestOf > 1 then
                        "Undo the last game"

                    else
                        "Undo the result"
                , onButtonClick <| Undo key
                ]
                []

          else
            text ""
        ]


{-| A part of the knockout view: the main bracket or a placement bracket.
`first` is the first place of a placement bracket. `localRound` gives the round
within the bracket for the rounds of its matches. `maxRound` is the first round
that is shown, `fullMaxRound` the first round of the bracket. The bracket takes
`rows` rows after `offset`.
-}
type alias ViewPart =
    { first : Maybe Int
    , localRound : Int -> Maybe Int
    , maxRound : Int
    , fullMaxRound : Int
    , offset : Int
    , rows : Int
    , matches : List ( MatchKey, Match )
    }


{-| The places of a placement bracket: a bracket with `matches` matches has one
more player.
-}
placesName : Int -> Int -> String
placesName first matches =
    String.fromInt first ++ "–" ++ String.fromInt (first + matches)


{-| Only editors see the buttons to undo a result. The matches keep their
numbers when the finished ones are hidden. A match of one game is decided by
clicking its winner, longer matches show the games won by each player.
`highlight` is the player whose path is highlighted.

The placement brackets follow below the main bracket in the order of their
places, each with a heading. All brackets end in the column of the final.

-}
viewGame : { names : Dict PlayerId String, canEdit : Bool, hideFinished : Bool, settings : Settings, highlight : Maybe PlayerId } -> Game -> Html Msg
viewGame { names, canEdit, hideFinished, settings, highlight } fullGame =
    let
        path : Path
        path =
            highlight
                |> Maybe.map (\id -> playerPath id fullGame)
                |> Maybe.withDefault (Path Set.empty Set.empty Set.empty)

        game : Game
        game =
            if hideFinished then
                hideFinishedMatches fullGame

            else
                fullGame

        -- pre-round matches are keyed by the main round slot they feed, so the keys can have gaps
        matchNumber : Int -> Int -> Int
        matchNumber round index =
            Dict.get round fullGame.rounds
                |> Maybe.andThen (Dict.keys >> List.Extra.elemIndex index)
                |> Maybe.withDefault index
                |> (+) 1

        itemsInRound : Int -> Int
        itemsInRound x =
            Bitwise.shiftLeftBy x 1

        maxOf : List Int -> Int
        maxOf =
            List.maximum >> Maybe.withDefault 0

        mainLocal : Int -> Maybe Int
        mainLocal round =
            if round >= 0 then
                Just round

            else
                Nothing

        bracketLocal : Int -> Int -> Maybe Int
        bracketLocal first round =
            placementBracketOf round
                |> Maybe.andThen
                    (\( bracketFirst, local ) ->
                        if bracketFirst == first then
                            Just local

                        else
                            Nothing
                    )

        localRounds : (Int -> Maybe Int) -> Game -> List Int
        localRounds localRound g =
            Dict.keys g.rounds |> List.filterMap localRound

        matchesOf : (Int -> Maybe Int) -> Game -> List ( MatchKey, Match )
        matchesOf localRound g =
            keyedMatches g |> List.filter (\( ( round, _ ), _ ) -> localRound round /= Nothing)

        hasThirdPlaceMatch : Bool
        hasThirdPlaceMatch =
            Dict.get 0 game.rounds
                |> Maybe.map (Dict.member 1)
                |> Maybe.withDefault False

        mainMaxRound : Int
        mainMaxRound =
            maxOf <| localRounds mainLocal game

        mainPart : ViewPart
        mainPart =
            { first = Nothing
            , localRound = mainLocal
            , maxRound = mainMaxRound
            , fullMaxRound = mainMaxRound
            , offset = 0
            , rows =
                if List.isEmpty (matchesOf mainLocal game) then
                    0

                else if mainMaxRound == 1 && hasThirdPlaceMatch then
                    6

                else
                    2 * itemsInRound mainMaxRound
            , matches = matchesOf mainLocal game
            }

        -- the first places of the placement brackets with shown matches
        brackets : List Int
        brackets =
            Dict.keys game.rounds
                |> List.filterMap (placementBracketOf >> Maybe.map Tuple.first)
                |> List.Extra.unique
                |> List.sort

        -- a placement bracket has an empty row and a row for its heading above it
        addBracket : Int -> ( Int, List ViewPart ) -> ( Int, List ViewPart )
        addBracket first ( offset, list ) =
            let
                localRound =
                    bracketLocal first

                maxRound =
                    maxOf <| localRounds localRound game
            in
            ( offset + 2 + 2 * itemsInRound maxRound
            , { first = Just first
              , localRound = localRound
              , maxRound = maxRound
              , fullMaxRound = maxOf <| localRounds localRound fullGame
              , offset = offset + 2
              , rows = 2 * itemsInRound maxRound
              , matches = matchesOf localRound game
              }
                :: list
            )

        ( totalRows, bracketParts ) =
            List.foldl addBracket ( mainPart.rows, [] ) brackets
                |> Tuple.mapSecond List.reverse

        parts : List ViewPart
        parts =
            mainPart :: bracketParts

        finalColumn : Int
        finalColumn =
            maxOf <| List.map .maxRound parts

        bracketMatches : Int -> Int
        bracketMatches first =
            List.length <| matchesOf (bracketLocal first) fullGame

        getBox : ViewPart -> Int -> Int -> { colStart : Int, colEnd : Int, rowStart : Int, rowEnd : Int }
        getBox part round index =
            let
                itemRoundSpan =
                    itemsInRound part.maxRound // itemsInRound round
            in
            if part.first == Nothing && round == 0 && index == 1 then
                { colStart = 2 * finalColumn + 1
                , colEnd = 2 * finalColumn + 2
                , rowStart = itemRoundSpan + 2
                , rowEnd = itemRoundSpan + 4
                }

            else
                { colStart = 2 * (finalColumn - round) + 1
                , colEnd = 2 * (finalColumn - round) + 2
                , rowStart = part.offset + (2 * index + 1) * itemRoundSpan
                , rowEnd = part.offset + (2 * index + 1) * itemRoundSpan + 2
                }

        roundNumber : ViewPart -> Int -> Int
        roundNumber part local =
            case part.first of
                Nothing ->
                    part.maxRound - local + 1

                Just _ ->
                    part.fullMaxRound - local + 1

        -- like "R2 M1" or "Places 5–8 R1 M2"
        matchName : MatchKey -> String
        matchName ( round, index ) =
            let
                inPart =
                    List.Extra.find (\part -> part.localRound round /= Nothing) parts

                name =
                    case inPart of
                        Just part ->
                            "R"
                                ++ String.fromInt (roundNumber part (Maybe.withDefault round <| part.localRound round))
                                ++ " M"
                                ++ String.fromInt (matchNumber round index)

                        Nothing ->
                            "M" ++ String.fromInt (matchNumber round index)
            in
            case placementBracketOf round of
                Just ( first, _ ) ->
                    placesName first (bracketMatches first) ++ " " ++ name

                Nothing ->
                    name

        -- the players of the placement brackets are the losers of other matches
        source : PlayerSlot -> Maybe String
        source slot =
            if Tuple.first slot < placementRound then
                keyedMatches fullGame
                    |> List.Extra.find (\( _, match ) -> match.loser == Just slot)
                    |> Maybe.map (\( key, _ ) -> "Loser " ++ matchName key)

            else
                Nothing

        viewSection : ViewPart -> MatchKey -> Match -> Html Msg
        viewSection part (( round, index ) as key) match =
            let
                local =
                    Maybe.withDefault round <| part.localRound round

                box =
                    getBox part local index
            in
            div
                [ class "game-section-wrapper"
                , HA.style "grid-column-start" <| String.fromInt box.colStart
                , HA.style "grid-column-end" <| String.fromInt box.colEnd
                , HA.style "grid-row-start" <| String.fromInt box.rowStart
                , HA.style "grid-row-end" <| String.fromInt box.rowEnd
                ]
                [ viewMatch
                    { names = names
                    , canEdit = canEdit
                    , bestOf = matchLength settings key
                    , title =
                        [ "Round " ++ String.fromInt (roundNumber part local)
                        , "Match " ++ String.fromInt (matchNumber round index)
                        ]
                    , playable = True
                    , highlight = path.slots
                    , source = source
                    }
                    game
                    key
                    match
                ]

        -- lines only within a bracket
        viewMover : ViewPart -> Bool -> MatchKey -> Match -> Html msg
        viewMover part loser ( round, index ) match =
            case
                if loser then
                    match.loser

                else
                    match.winner
            of
                Nothing ->
                    text ""

                Just (( targetRound, targetIndex ) as target) ->
                    case ( part.localRound round, part.localRound targetRound ) of
                        ( Just local, Just targetLocal ) ->
                            viewMoverTo part loser local index target ( targetLocal, targetIndex )

                        _ ->
                            text ""

        viewMoverTo : ViewPart -> Bool -> Int -> Int -> PlayerSlot -> PlayerSlot -> Html msg
        viewMoverTo part loser round index target ( targetRound, targetIndex ) =
            let
                box =
                    getBox part round index

                tbox =
                    getBox part targetRound <| targetIndex // 2

                vStart =
                    min box.rowStart tbox.rowStart + 1

                vEnd =
                    max box.rowEnd tbox.rowEnd - 1
            in
            div
                [ HA.classList
                    [ ( "game-mover-wrapper", True )
                    , ( if modBy 2 index == 0 then
                            "even"

                        else
                            "odd"
                      , True
                      )
                    , Tuple.pair "taken" <|
                        (/=) Nothing <|
                            Dict.get target game.slots
                    , Tuple.pair "killed" <|
                        Maybe.withDefault False <|
                            Maybe.map (.alive >> not) <|
                                Maybe.andThen
                                    (\id -> Dict.get id game.players)
                                <|
                                    Dict.get target game.slots
                    , Tuple.pair "highlight" <| Set.member target path.lines
                    , Tuple.pair "lose" loser
                    , Tuple.pair "lose1" <| loser && modBy 2 index == 0
                    , Tuple.pair "lose2" <| loser && modBy 2 index == 1
                    , Tuple.pair "down" <| box.rowStart < tbox.rowStart
                    , Tuple.pair "up" <| box.rowStart > tbox.rowStart
                    ]
                , HA.style "grid-column-start" <| String.fromInt <| box.colStart + 1
                , HA.style "grid-column-end" <| String.fromInt <| box.colEnd + 1
                , HA.style "grid-row-start" <|
                    String.fromInt <|
                        if loser && modBy 2 index == 0 then
                            vStart - 1

                        else
                            vStart
                , HA.style "grid-row-end" <|
                    String.fromInt <|
                        if loser && modBy 2 index == 1 then
                            vEnd + 1

                        else
                            vEnd
                ]
            <|
                List.repeat 3 <|
                    div [] []

        {- The shown match with the slot, its part, its local round and whether
           the slot is its second player. A player with a bye keeps the slot of
           the pre-round, but plays in the match of the next round.
        -}
        targetIn : PlayerSlot -> Maybe { part : ViewPart, local : Int, index : Int, second : Bool }
        targetIn slot =
            parts
                |> List.Extra.findMap
                    (\part ->
                        part.matches
                            |> List.Extra.findMap
                                (\( ( round, index ), match ) ->
                                    if match.player1 == slot || match.player2 == slot then
                                        part.localRound round
                                            |> Maybe.map
                                                (\local ->
                                                    { part = part
                                                    , local = local
                                                    , index = index
                                                    , second = match.player2 == slot
                                                    }
                                                )

                                    else
                                        Nothing
                                )
                    )

        {- The line of the highlighted player from the match they lost into a
           placement bracket. It leaves at the loser, turns down right after the
           match and enters the target box at the row of the slot. The brackets
           are stacked so that it crosses no other bracket.
        -}
        viewDrop : ViewPart -> MatchKey -> Match -> Html msg
        viewDrop part ( round, index ) match =
            case ( match.loser, part.localRound round ) of
                ( Just target, Just local ) ->
                    case targetIn target of
                        Just to ->
                            if Set.member target path.lines && to.part.first /= part.first then
                                let
                                    box =
                                        getBox part local index

                                    tbox =
                                        getBox to.part to.local to.index

                                    top =
                                        min box.rowStart tbox.rowStart

                                    rows =
                                        max box.rowEnd tbox.rowEnd - top

                                    leave =
                                        if Dict.get target game.slots == Dict.get match.player2 game.slots then
                                            1

                                        else
                                            -1

                                    enter =
                                        if to.second then
                                            1

                                        else
                                            -1

                                    -- a box spans two rows, its middle is the line between them
                                    css : Int -> Float -> String
                                    css boxStart em =
                                        "calc("
                                            ++ String.fromFloat (100 * toFloat (boxStart + 1 - top) / toFloat rows)
                                            ++ "% + "
                                            ++ String.fromFloat (em - 0.2)
                                            ++ "em)"

                                    killed =
                                        Dict.get target game.slots
                                            |> Maybe.andThen (\id -> Dict.get id game.players)
                                            |> Maybe.map (.alive >> not)
                                            |> Maybe.withDefault False
                                in
                                div
                                    [ HA.classList
                                        [ ( "knockout-drop", True )
                                        , ( "killed", killed )
                                        ]
                                    , HA.style "grid-column" <| String.fromInt box.colEnd ++ " / " ++ String.fromInt tbox.colStart
                                    , HA.style "grid-row" <| String.fromInt top ++ " / " ++ String.fromInt (top + rows)
                                    ]
                                    [ div
                                        [ class "knockout-drop-across"
                                        , HA.style "left" "0"
                                        , HA.style "width" "1.2em"
                                        , HA.style "top" <| css box.rowStart leave
                                        ]
                                        []
                                    , div
                                        [ class "knockout-drop-down"
                                        , HA.style "left" "1em"
                                        , HA.style "top" <| css box.rowStart leave
                                        , HA.style "height" <|
                                            "calc("
                                                ++ css tbox.rowStart enter
                                                ++ " - "
                                                ++ css box.rowStart leave
                                                ++ " + 0.4em)"
                                        ]
                                        []
                                    , div
                                        [ class "knockout-drop-across"
                                        , HA.style "left" "1.2em"
                                        , HA.style "right" "0"
                                        , HA.style "top" <| css tbox.rowStart enter
                                        ]
                                        []
                                    ]

                            else
                                text ""

                        Nothing ->
                            text ""

                _ ->
                    text ""

        viewLabel : ViewPart -> Html msg
        viewLabel part =
            case part.first of
                Just first ->
                    div
                        [ class "knockout-label"
                        , HA.style "grid-column" <| String.fromInt (2 * (finalColumn - part.maxRound) + 1) ++ " / -1"
                        , HA.style "grid-row" <| String.fromInt part.offset ++ " / " ++ String.fromInt (part.offset + 1)
                        ]
                        [ text <| "Places " ++ placesName first (bracketMatches first) ]

                Nothing ->
                    text ""

        viewPart : ViewPart -> List (Html Msg)
        viewPart part =
            viewLabel part
                :: List.concatMap
                    (\( key, match ) ->
                        [ viewSection part key match
                        , viewMover part True key match
                        , viewMover part False key match
                        , viewDrop part key match
                        ]
                    )
                    part.matches
    in
    div
        [ class "game"
        , onMouse "pointerleave" <| Highlight LeaveBracket
        , onTap <| Highlight TapEmpty
        , HA.style "grid-template-rows" <|
            "repeat("
                ++ String.fromInt (max 1 totalRows)
                ++ ", 1fr)"
        , HA.style "grid-template-columns" <|
            if finalColumn > 0 then
                "repeat(" ++ String.fromInt finalColumn ++ ", 3fr 1fr) 3fr"

            else
                "3fr"
        ]
    <|
        List.concatMap viewPart parts


{-| The winner moves on once they have won enough games.
-}
update : Settings -> Msg -> Game -> Game
update settings =
    updateWith
        { matchLength = matchLength settings
        , playable = \_ _ -> True
        }


{-| The rules of a bracket built from knockout matches. Only `playable`
matches take games and walkovers.
-}
type alias Rules =
    { matchLength : MatchKey -> BestOf
    , playable : MatchKey -> Game -> Bool
    }


updateWith : Rules -> Msg -> Game -> Game
updateWith rules msg game =
    case msg of
        WinGame isPlayer2 key ->
            case matchAt key game of
                Just match ->
                    if rules.playable key game && canSetWinner match game then
                        let
                            bestOf : BestOf
                            bestOf =
                                rules.matchLength key

                            games : Games
                            games =
                                BestOf.addGame bestOf isPlayer2 match.games
                        in
                        case BestOf.winner bestOf games of
                            Just winner ->
                                setGames key games game
                                    |> decide winner match
                                    |> resolveRemovedWhere rules.playable

                            Nothing ->
                                setGames key games game

                    else
                        game

                Nothing ->
                    game

        Undo key ->
            case matchAt key game of
                Just match ->
                    if canUndo match game then
                        (if isDecided match game then
                            undoDecision match game

                         else
                            game
                        )
                            |> setGames key (BestOf.removeLastGame match.games)

                    else
                        game

                Nothing ->
                    game

        Highlight _ ->
            game
