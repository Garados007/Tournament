module Bracket.Knockout exposing (..)

import Bitwise
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import List.Extra
import PlayerId exposing (PlayerId)


{-| A place a player can occupy: the round and the index within it. A player
occupies one more slot for every match they win.

Round 0 is the final, higher rounds come earlier. Round -1 holds the placement
slots.

-}
type alias PlayerSlot =
    ( Int, Int )


{-| A match between the players in `player1` and `player2`. The winner and the
loser move to the given slots of a later match. Only the semi-finals and, with
placements, the final have a loser slot.
-}
type alias Match =
    { player1 : PlayerSlot
    , player2 : PlayerSlot
    , winner : Maybe PlayerSlot
    , loser : Maybe PlayerSlot
    }


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


type alias Settings =
    { thirdPlace : ThirdPlace
    }


{-| How the losers of the semi-finals are placed.
-}
type ThirdPlace
    = Playoff
    | Shared
    | NoThirdPlace


defaultSettings : Settings
defaultSettings =
    { thirdPlace = Playoff }


type Msg
    = SetWinner Bool Match
    | Undo Match


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
                |> Maybe.andThen .winner
                |> Maybe.map (\slot -> Dict.member slot game.slots)
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
                                    { player1 = mapper match.player1
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


{-| A result can be undone as long as the matches its players moved on to have
no result. Matches with a removed player can't be undone, as that would bring
the player back.
-}
canUndo : Match -> Game -> Bool
canUndo match game =
    case ( match.winner, opponents match game ) of
        ( Just _, Just ( id1, id2 ) ) ->
            isDecided match game
                && not (isRemoved id1 game)
                && not (isRemoved id2 game)
                && ([ match.winner, match.loser ]
                        |> List.filterMap identity
                        |> List.filterMap (\slot -> matchWithInput slot game)
                        |> List.all (\next -> not <| isDecided next game)
                   )

        _ ->
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
                    if round >= 0 || not (isRemoved id game) then
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
undo : Match -> Game -> Game
undo match game =
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


{-| Decides every open match between a removed player and one who is still in.
-}
resolveRemoved : Game -> Game
resolveRemoved game =
    let
        walkover : Match -> Maybe ( Bool, Match )
        walkover match =
            case opponents match game of
                Just ( id1, id2 ) ->
                    if match.winner == Nothing || isDecided match game then
                        Nothing

                    else if isRemoved id1 game && not (isRemoved id2 game) then
                        Just ( True, match )

                    else if isRemoved id2 game && not (isRemoved id1 game) then
                        Just ( False, match )

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    case List.Extra.findMap walkover (allMatches game) of
        Just ( isPlayer2, match ) ->
            resolveRemoved <| decide isPlayer2 match game

        Nothing ->
            game


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


{-| Only editors see the buttons to undo a result. The matches keep their
numbers when the finished ones are hidden.
-}
viewGame : { names : Dict PlayerId String, canEdit : Bool, hideFinished : Bool } -> Game -> Html Msg
viewGame { names, canEdit, hideFinished } fullGame =
    let
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

        maxRound : Int
        maxRound =
            Dict.keys game.rounds |> List.maximum |> Maybe.withDefault 0

        hasThirdPlaceMatch : Bool
        hasThirdPlaceMatch =
            Dict.get 0 game.rounds
                |> Maybe.map (Dict.member 1)
                |> Maybe.withDefault False

        itemsInRound : Int -> Int
        itemsInRound x =
            Bitwise.shiftLeftBy x 1

        itemRoundSpan : Int -> Int
        itemRoundSpan round =
            itemsInRound maxRound // itemsInRound round

        getBox : Int -> Int -> { colStart : Int, colEnd : Int, rowStart : Int, rowEnd : Int }
        getBox round index =
            if round == 0 && index == 1 then
                { colStart = (\x -> x * 2 - 1) <| maxRound + 1
                , colEnd = (\x -> x * 2 - 2) <| maxRound + 2
                , rowStart = itemRoundSpan 0 + 2
                , rowEnd = itemRoundSpan 0 + 4
                }

            else
                { colStart = (\x -> x * 2 - 1) <| maxRound - round + 1
                , colEnd = (\x -> x * 2 - 2) <| maxRound - round + 2
                , rowStart = (2 * index + 1) * itemRoundSpan round
                , rowEnd = (2 * index + 1) * itemRoundSpan round + 2
                }

        viewMatchPlayer : Bool -> Match -> Html Msg
        viewMatchPlayer isPlayer2 match =
            div [ class "game-match-player" ]
                [ div [ class "game-match-player-name" ] <|
                    Maybe.withDefault [] <|
                        Maybe.map (List.singleton << viewPlayer names) <|
                            Maybe.andThen (\id -> Dict.get id game.players) <|
                                Dict.get
                                    (if isPlayer2 then
                                        match.player2

                                     else
                                        match.player1
                                    )
                                <|
                                    game.slots
                , div
                    [ HA.classList
                        [ ( "game-match-winner", True )
                        , ( "enabled", canSetWinner match game )
                        ]
                    , HA.title "Select as winner"
                    , HE.onClick <| SetWinner isPlayer2 match
                    ]
                    []
                ]

        viewSection : Int -> Int -> Match -> Html Msg
        viewSection round index match =
            let
                box =
                    getBox round index
            in
            div
                [ class "game-section-wrapper"
                , HA.style "grid-column-start" <| String.fromInt box.colStart
                , HA.style "grid-column-end" <| String.fromInt box.colEnd
                , HA.style "grid-row-start" <| String.fromInt box.rowStart
                , HA.style "grid-row-end" <| String.fromInt box.rowEnd
                ]
                [ div [ class "game-section" ]
                    [ div [ class "game-section-title" ]
                        [ div []
                            [ text <| "Round " ++ String.fromInt (maxRound - round + 1) ]
                        , div []
                            [ text <| "Match " ++ String.fromInt (matchNumber round index) ]
                        ]
                    , viewMatchPlayer False match
                    , viewMatchPlayer True match
                    , if canEdit && canUndo match game then
                        div
                            [ class "game-match-undo"
                            , HA.title "Undo the result"
                            , HE.onClick <| Undo match
                            ]
                            []

                      else
                        text ""
                    ]
                ]

        viewMover : Bool -> Int -> Int -> Match -> Html msg
        viewMover loser round index match =
            case
                if loser then
                    match.loser

                else
                    match.winner
            of
                Nothing ->
                    text ""

                Just target ->
                    if Tuple.first target < 0 then
                        text ""

                    else
                        viewMoverTo loser round index target

        viewMoverTo : Bool -> Int -> Int -> PlayerSlot -> Html msg
        viewMoverTo loser round index (( targetRound, targetIndex ) as target) =
            let
                box =
                    getBox round index

                tbox =
                    getBox targetRound <| targetIndex // 2

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
    in
    div
        [ class "game"
        , HA.style "grid-template-rows" <|
            "repeat("
                ++ String.fromInt
                    ((*) 2 <|
                        if maxRound == 1 && hasThirdPlaceMatch then
                            3

                        else
                            itemsInRound maxRound
                    )
                ++ ", 1fr)"
        , HA.style "grid-template-columns" <|
            if maxRound > 0 then
                "repeat(" ++ String.fromInt maxRound ++ ", 3fr 1fr) 3fr"

            else
                "3fr"
        ]
    <|
        List.concatMap
            (\( round, x ) ->
                List.concatMap
                    (\( index, match ) ->
                        [ viewSection round index match
                        , viewMover True round index match
                        , viewMover False round index match
                        ]
                    )
                <|
                    Dict.toList x
            )
        <|
            Dict.toList game.rounds


update : Msg -> Game -> Game
update msg game =
    case msg of
        SetWinner isPlayer2 match ->
            if canSetWinner match game then
                decide isPlayer2 match game
                    |> resolveRemoved

            else
                game

        Undo match ->
            if canUndo match game then
                undo match game

            else
                game
