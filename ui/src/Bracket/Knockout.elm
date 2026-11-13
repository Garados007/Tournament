module Bracket.Knockout exposing (..)

import Bitwise
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import List.Extra


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


type alias Player id =
    { id : id
    , name : String
    , alive : Bool
    }


type alias Game id =
    { rounds : Dict Int Round
    , slots : Dict PlayerSlot id
    , players : Dict id (Player id)
    }


type Msg
    = SetWinner Bool Match


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


generateTournament : Int -> Game player
generateTournament playerCount =
    let
        countRounds : Int -> Int -> Int
        countRounds pc l =
            if pc > 1 then
                countRounds (pc // 2) (l + 1)

            else
                l

        maxRound : Int
        maxRound =
            countRounds playerCount -1

        createRounds : Int -> Int -> List ( Int, Round )
        createRounds pc round =
            if round < 0 then
                []

            else
                ( round, generateRound pc round ) :: createRounds (pc // 2) (round - 1)

        limit : Int
        limit =
            Bitwise.shiftLeftBy maxRound 2

        preTournament : Maybe ( Int, Round )
        preTournament =
            if limit == playerCount then
                Nothing

            else
                Just ( maxRound + 1, generateRound (2 * (playerCount - limit)) (maxRound + 1) )

        preTournamentSlots : List PlayerSlot
        preTournamentSlots =
            case preTournament of
                Nothing ->
                    []

                Just ( _, p ) ->
                    List.filterMap .winner <| Dict.values p

        moveSlot : PlayerSlot -> PlayerSlot
        moveSlot slot =
            if List.member slot preTournamentSlots then
                slot

            else
                ( maxRound + 1, Tuple.second slot + (playerCount - limit) )

        gamefy : List ( Int, Round ) -> Game player
        gamefy rounds =
            Game
                (Dict.fromList rounds)
                Dict.empty
                Dict.empty

        gameForThird : Game player -> Game player
        gameForThird game =
            if playerCount >= 4 then
                { game
                    | rounds =
                        game.rounds
                            |> Dict.update 1
                                (Maybe.map <|
                                    Dict.map <|
                                        \index match ->
                                            { match
                                                | loser = Just ( 0, 2 + index )
                                            }
                                )
                            |> Dict.update 0
                                (Maybe.map <|
                                    Dict.insert 1
                                        { player1 = ( 0, 2 )
                                        , player2 = ( 0, 3 )
                                        , winner = Nothing
                                        , loser = Nothing
                                        }
                                )
                }

            else
                game
    in
    gameForThird <|
        case ( preTournament, createRounds limit maxRound ) of
            ( Nothing, matches ) ->
                gamefy matches

            ( _, [] ) ->
                gamefy []

            ( Just t, ( id, p ) :: ps ) ->
                gamefy <|
                    t
                        :: (Tuple.pair id <|
                                Dict.map
                                    (\_ match ->
                                        { match
                                            | player1 = moveSlot match.player1
                                            , player2 = moveSlot match.player2
                                        }
                                    )
                                    p
                           )
                        :: ps


playerSlotToString : PlayerSlot -> String
playerSlotToString ( round, index ) =
    "(" ++ String.fromInt round ++ ", " ++ String.fromInt index ++ ")"


initialSlots : Game id -> List PlayerSlot
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


setInitialPlayers : List ( comparableId, String ) -> Game comparableId -> Game comparableId
setInitialPlayers list game =
    { game
        | players =
            Dict.fromList list
                |> Dict.map
                    (\id name -> Player id name True)
        , slots =
            List.map Tuple.first list
                |> List.Extra.zip (initialSlots game)
                |> Dict.fromList
    }


addPosition : Game id -> Maybe (Game id)
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

        updateSlot : (PlayerSlot -> PlayerSlot) -> Game id
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
        |> Debug.log "availablePos"
        |> Maybe.map
            (\(( round, index ) as pos) ->
                posToMatch pos
                    |> Debug.log "new match"
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
                                                case Debug.log "remove old slot" <| Dict.get ( round - 1, index ) game_.slots of
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


isPlayerUsed : id -> Game id -> Bool
isPlayerUsed id game =
    Dict.values game.slots
        |> List.member id


{-| Creates a tournament with the players in list order.
-}
start : List ( comparableId, String ) -> Game comparableId
start list =
    setInitialPlayers list <|
        generateTournament <|
            List.length list


{-| Registers a new player and adds a match for it. Without a free position the
player is only registered.
-}
addPlayer : comparableId -> String -> Game comparableId -> Game comparableId
addPlayer id name game =
    let
        withPlayer : Game comparableId
        withPlayer =
            { game
                | players =
                    Dict.insert id
                        { id = id, name = name, alive = True }
                        game.players
            }
    in
    addPosition withPlayer
        |> Maybe.withDefault withPlayer


renamePlayer : comparableId -> String -> Game comparableId -> Game comparableId
renamePlayer id name game =
    { game
        | players =
            Dict.update id
                (Maybe.map <|
                    \player ->
                        { player | name = name }
                )
                game.players
    }


{-| Marks the player as out. Opponents who already wait in a match against them
move on.
-}
removePlayer : comparableId -> Game comparableId -> Game comparableId
removePlayer id game =
    let
        playerAt : PlayerSlot -> Maybe comparableId
        playerAt slot =
            Dict.get slot game.slots

        opponentMovesOn : Match -> Maybe ( PlayerSlot, comparableId )
        opponentMovesOn match =
            case match.winner of
                Just winner ->
                    if playerAt winner /= Nothing then
                        Nothing

                    else if playerAt match.player1 == Just id then
                        playerAt match.player2 |> Maybe.map (Tuple.pair winner)

                    else if playerAt match.player2 == Just id then
                        playerAt match.player1 |> Maybe.map (Tuple.pair winner)

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    { game
        | players =
            Dict.update id
                (Maybe.map <| \player -> { player | alive = False })
                game.players
        , slots =
            Dict.values game.rounds
                |> List.concatMap Dict.values
                |> List.filterMap opponentMovesOn
                |> List.foldl (\( slot, opponent ) -> Dict.insert slot opponent) game.slots
    }


{-| Whether the player is registered and out. Unknown players aren't out.
-}
isEliminated : comparableId -> Game comparableId -> Bool
isEliminated id game =
    Dict.get id game.players
        |> Maybe.map (.alive >> not)
        |> Maybe.withDefault False


hideFinishedMatches : Game id -> Game id
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


viewPlayer : Player id -> Html msg
viewPlayer player =
    div
        [ HA.classList
            [ ( "player", True )
            , ( "alive", player.alive )
            ]
        ]
        [ text player.name
        ]


viewGame : Game comparableId -> Html Msg
viewGame game =
    let
        maxRound : Int
        maxRound =
            Dict.keys game.rounds |> List.maximum |> Maybe.withDefault 0

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
                        Maybe.map (List.singleton << viewPlayer) <|
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
                        , Tuple.pair "enabled" <|
                            Nothing
                                /= Dict.get
                                    (if isPlayer2 then
                                        match.player2

                                     else
                                        match.player1
                                    )
                                    game.slots
                                && (case match.winner of
                                        Nothing ->
                                            True

                                        Just winner ->
                                            Dict.get winner game.slots == Nothing
                                   )
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
                            [ text <| "Match " ++ String.fromInt (index + 1) ]
                        ]
                    , viewMatchPlayer False match
                    , viewMatchPlayer True match
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

                Just (( targetRound, targetIndex ) as target) ->
                    let
                        box =
                            getBox round index

                        tbox =
                            getBox targetRound <| targetIndex // 2

                        vStart =
                            min box.rowStart tbox.rowStart + 1

                        vEnd =
                            max box.rowEnd tbox.rowEnd - 1

                        d_ =
                            if modBy 2 index == 1 then
                                always 1 <|
                                    Debug.log "mover"
                                        { box = box
                                        , tbox = box
                                        , left = ( round, index )
                                        , right = target
                                        }

                            else
                                1
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
                        if maxRound == 1 then
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
                        if round == 0 then
                            [ viewSection round index match ]

                        else
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


update : Msg -> Game comparableId -> Game comparableId
update msg game =
    case msg of
        SetWinner isPlayer2 match ->
            let
                winnerSlot : PlayerSlot
                winnerSlot =
                    if isPlayer2 then
                        match.player2

                    else
                        match.player1

                loserSlot : PlayerSlot
                loserSlot =
                    Debug.log "loserSlot" <|
                        if isPlayer2 then
                            match.player1

                        else
                            match.player2

                loserId : Maybe comparableId
                loserId =
                    Debug.log "loserId" <| Dict.get loserSlot game.slots

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
                        |> insertIfGiven match.loser (Dict.get loserSlot game.slots)
                , players =
                    -- a loser with a loser slot moves on to the third-place match or the second place
                    if match.loser /= Nothing then
                        game.players

                    else
                        Maybe.map
                            (\id ->
                                Dict.update id
                                    (Maybe.map (\x -> { x | alive = False }))
                                    game.players
                            )
                            loserId
                            |> Maybe.withDefault game.players
            }
