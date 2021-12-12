module Bracket.KO exposing (..)

import Bitwise
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Debug.Extra
import List.Extra


{-|
The slot that the player currently occupy. A player can occupy many slots if a wins one or more
rounds.

This first item is the level, the second the index.
-}
type alias PlayerSlot = (Int, Int)

type alias Phase =
    { player1: PlayerSlot -- index of player of the input level
    , player2: PlayerSlot -- index of player of the input level
    , winner: Maybe PlayerSlot -- index of player of the next input level
    , looser: Maybe PlayerSlot -- index of player of the next input level
    }

type alias PhaseLevel = Dict Int Phase

type alias Player id =
    { id: id
    , name: String
    , alive: Bool
    }

type alias Game id =
    { levels: Dict Int PhaseLevel
    , slots: Dict PlayerSlot id
    , player: Dict id (Player id)
    }

type Msg
    = SetWinner Bool Phase

generateLevel : Int -> Int -> PhaseLevel
generateLevel playerCount level =
    Dict.fromList
    <| List.map
        (\i -> Tuple.pair i
            { player1 = (level, i * 2)
            , player2 = (level, i * 2 + 1)
            , winner = 
                if level > 0
                then Just (level - 1, i)
                else Nothing 
            , looser = Nothing
            }
        )
    <| List.range 0 (playerCount // 2 - 1)

generateTournament : Int -> Game player
generateTournament playerCount =
    let
        getLevel : Int -> Int -> Int
        getLevel pc l =
            if pc > 1
            then getLevel (pc // 2) (l + 1)
            else l

        maxLevel : Int
        maxLevel = getLevel playerCount -1

        createPhases : Int -> Int -> List (Int, PhaseLevel)
        createPhases pc level =
            if level < 0
            then []
            else (level, generateLevel pc level) :: createPhases (pc // 2) (level - 1)
        
        limit : Int
        limit = Bitwise.shiftLeftBy maxLevel 2

        preTournament : Maybe (Int, PhaseLevel)
        preTournament =
            if limit == playerCount
            then Nothing
            else Just (maxLevel + 1, generateLevel (2 * (playerCount - limit)) (maxLevel + 1))
        
        preTournamentSlots : List PlayerSlot
        preTournamentSlots =
            case preTournament of
                Nothing -> []
                Just (_, p) ->
                    List.filterMap .winner <| Dict.values p
        
        moveSlot : PlayerSlot -> PlayerSlot
        moveSlot slot =
            if List.member slot preTournamentSlots
            then slot
            else (maxLevel + 1, Tuple.second slot + (playerCount - limit))

        gamefy : List (Int, PhaseLevel) -> Game player
        gamefy levels = Game
            (Dict.fromList levels)
            Dict.empty
            Dict.empty

    in case (preTournament, createPhases limit maxLevel) of
        (Nothing, phases) -> gamefy phases
        (_, []) -> gamefy []
        (Just t, (id, p)::ps) -> gamefy <|
            t ::
            (Tuple.pair id
                <| Dict.map
                    (\_ phase ->
                        { phase
                        | player1 = moveSlot phase.player1
                        , player2 = moveSlot phase.player2
                        }
                    )
                    p
            ):: ps

playerSlotToString : PlayerSlot -> String
playerSlotToString (level, index) = "(" ++ String.fromInt level ++ ", " ++ String.fromInt index ++ ")"

initialSlots : Game id -> List PlayerSlot
initialSlots game =
    let
        maxLevel : Int
        maxLevel = Dict.keys game.levels |> List.maximum |> Maybe.withDefault 0


    in game.levels
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concatMap
            (\phase -> [ phase.player1, phase.player2 ])
        |> List.filter
            (\(level, _) -> level == maxLevel)

setInitialPlayer : List (comparableId, String) -> Game comparableId -> Game comparableId
setInitialPlayer list game =
    { game
    | player =
        Dict.fromList list
        |> Dict.map
            (\id name -> Player id name True)
    , slots = List.map Tuple.first list
        |> List.Extra.zip (initialSlots game)
        |> Dict.fromList
    }

addPosition : Game id -> Maybe (Game id)
addPosition game =
    let
        maxKey : Dict Int value -> Int
        maxKey = Dict.keys >> List.maximum >> Maybe.withDefault 0

        maxLevel = maxKey game.levels

        maxLevelPlacement : Int -> Int
        maxLevelPlacement n = Bitwise.shiftLeftBy n 1

        allPositions : List (Int, Int)
        allPositions =
            List.concat
                [ List.map (Tuple.pair maxLevel)
                    <| List.range 0 
                    <| maxLevelPlacement maxLevel - 1
                , List.map (Tuple.pair <| maxLevel + 1)
                    <| List.range 0
                    <| maxLevelPlacement (maxLevel + 1) - 1
                ]

        follower : (Int, Int) -> (Int, Int)
        follower (level, index) = (level - 1, index // 2)

        posExists : (Int, Int) -> Bool
        posExists (level, index) =
            Dict.get level game.levels
            |> Maybe.map (Dict.member index)
            |> Maybe.withDefault False

        posLocked : (Int, Int) -> Bool
        posLocked (level, index) =
            Dict.get level game.levels
            |> Maybe.andThen (Dict.get index)
            |> Maybe.andThen .winner
            |> Maybe.map (\slot -> Dict.member slot game.slots)
            |> Maybe.withDefault False

        availablePos : List (Int, Int)
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
        
        getPhase : (Int, Int) -> Maybe Phase
        getPhase (level, index) =
            Dict.get level game.levels
            |> Maybe.andThen (Dict.get index)
        
        posToPhase : (Int, Int) -> (PlayerSlot, Phase)
        posToPhase (level, index) =
            -- Step 1: get the input slot of the following pos
            getPhase (follower (level, index))
            |> Maybe.map
                (\phase ->
                    if modBy 2 index == 0
                    then phase.player1
                    else phase.player2
                )
            -- Step 1.1: this case should not happen (we have checked before that the follower exists) 
            |> Maybe.withDefault (level, 0)
            -- Step 2: create a phase for it
            |> \slot -> Tuple.pair slot
                { player1 = (level, Tuple.second slot)
                , player2 = (level, Tuple.second slot + 1)
                , winner = Just <| (level - 1, index)
                , looser = Nothing
                }
        
        updateSlot : (PlayerSlot -> PlayerSlot) -> Game id
        updateSlot mapper =
            { game
            | levels =
                Dict.map
                    (\_ ->
                        Dict.map
                            (\_ phase ->
                                { player1 = mapper phase.player1
                                , player2 = mapper phase.player2
                                , winner = Maybe.map mapper phase.winner
                                , looser = Maybe.map mapper phase.looser
                                }
                            )
                    )
                    game.levels
            , slots = Dict.toList game.slots
                |> List.map (Tuple.mapFirst mapper)
                |> Dict.fromList
            }

        insertSlot : PlayerSlot -> PlayerSlot -> PlayerSlot -> PlayerSlot
        insertSlot (il, ii) new (vl, vi) =
            if il == vl
            then
                if ii > vi
                then (vl, vi)
                else
                    if ii < vi
                    then (vl, vi + 1)
                    else new
            else (vl, vi)

        addSlotLevel : PlayerSlot -> PlayerSlot
        addSlotLevel (level, index) =
            if level == maxLevel
            then (level + 1, index)
            else (level, index)
    in List.head availablePos
        |> Debug.log "availablePos"
        |> Maybe.map
            (\((level, index) as pos) ->
                posToPhase pos
                |> Debug.log "new phase"
                |> \(slot, phase) ->
                    updateSlot
                        ( if level == maxLevel
                            then insertSlot slot (level - 1, index)
                            else addSlotLevel
                                >> insertSlot
                                    (level, Tuple.second slot)
                                    (level - 1, index)
                        )
                |> \game_ ->
                    { game_
                    | levels = Dict.update level
                        (Maybe.withDefault Dict.empty
                            >> Dict.insert index phase
                            >> Just
                        )
                        game_.levels
                    , slots = case Debug.log "remove old slot" <| Dict.get (level - 1, index) game_.slots of
                        Just x -> game_.slots
                            |> Dict.remove (level - 1, index)
                            |> Dict.insert (level, Tuple.second slot) x
                        Nothing -> game_.slots
                    }
                |> Tuple.pair (level, Tuple.second slot)
            )
        |> Maybe.map
            (\(slot, game_) ->
                Dict.keys game_.player
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

hideFinishedPhases : Game id -> Game id
hideFinishedPhases game =
    { game
    | levels = game.levels
        |> Dict.map
            (\_ -> Dict.filter
                <| \_ phase -> phase.winner
                    |> Maybe.andThen (\x -> Dict.get x game.slots)
                    |> (==) Nothing
            )
        |> Dict.filter (\_ -> Dict.isEmpty >> not)
    }

viewPlayer : Player id -> Html msg
viewPlayer player =
    div [ HA.classList
            [ ("player", True)
            , ("alive", player.alive)
            ]
        ]
        [ text player.name
        ]

viewGame : Game comparableId -> Html Msg
viewGame game =
    let
        maxLevel : Int
        maxLevel = Dict.keys game.levels |> List.maximum |> Maybe.withDefault 0

        itemsOnLevel : Int -> Int
        itemsOnLevel x = Bitwise.shiftLeftBy x 1

        itemLevelSpan : Int -> Int
        itemLevelSpan level = itemsOnLevel maxLevel // itemsOnLevel level

        getBox : Int -> Int -> { colStart: Int, colEnd: Int, rowStart: Int, rowEnd: Int }
        getBox level index =
            { colStart = (\x -> x * 2 - 1) <| maxLevel - level + 1
            , colEnd = (\x -> x * 2 - 2) <| maxLevel - level + 2
            , rowStart = index * itemLevelSpan level + 1
            , rowEnd = (index + 1) * itemLevelSpan level + 1
            }
        
        viewBattleBox : Bool -> Phase -> Html Msg
        viewBattleBox isPlayer2 phase =
            div [ class "game-battle-box" ]
                [ div [ class "game-battle-player-wrapper" ]
                    <| Maybe.withDefault []
                    <| Maybe.map (List.singleton << viewPlayer)
                    <| Maybe.andThen (\id -> Dict.get id game.player)
                    <| Dict.get 
                        (if isPlayer2
                        then phase.player2
                        else phase.player1
                        )
                    <| game.slots
                , div 
                    [ HA.classList
                        [ ("game-battle-winner", True)
                        , Tuple.pair "enabled"
                            <| Nothing /= Dict.get 
                                (if isPlayer2
                                then phase.player2
                                else phase.player1
                                )
                                game.slots
                            &&  ( case phase.winner of
                                    Nothing -> True
                                    Just winner -> Dict.get winner game.slots == Nothing
                                )
                        ]
                    , HA.title "Select as Winner"
                    , HE.onClick <| SetWinner isPlayer2 phase
                    ]
                    [

                    ]
                ]

        viewSection : Int -> Int -> Phase -> Html Msg
        viewSection level index phase =
            let 
                box = getBox level index
            in div
                [ class "game-section-wrapper"
                , HA.style "grid-column-start" <| String.fromInt box.colStart
                , HA.style "grid-column-end" <| String.fromInt box.colEnd
                , HA.style "grid-row-start" <| String.fromInt box.rowStart
                , HA.style "grid-row-end" <| String.fromInt box.rowEnd
                ]
                [ div [ class "game-section" ]
                    [ div [ class "game-section-title" ]
                        [ div []
                            [ text <| "Phase: " ++ String.fromInt (maxLevel - level + 1) ]
                        , div []
                            [ text <| "Battle: " ++ String.fromInt (index + 1) ]
                        ]
                    , viewBattleBox False phase
                    , viewBattleBox True phase
                    ]
                ]
        
        viewMover : Int -> Int -> Phase -> Html msg
        viewMover level index phase =
            let
                box = getBox level index
            in div
                [ HA.classList
                    [ ("game-mover-wrapper", True)
                    , (if modBy 2 index == 0 then "even" else "odd", True)
                    , Tuple.pair "taken"
                        <| (/=) Nothing
                        <| Maybe.andThen
                            (\slot -> Dict.get slot game.slots)
                        <| phase.winner
                    , Tuple.pair "killed"
                        <| Maybe.withDefault False
                        <| Maybe.map (.alive >> not)
                        <| Maybe.andThen
                            (\id -> Dict.get id game.player)
                        <| Maybe.andThen
                            (\slot -> Dict.get slot game.slots)
                        <| phase.winner
                    ]
                , HA.style "grid-column-start" <| String.fromInt <| box.colStart + 1
                , HA.style "grid-column-end" <| String.fromInt <| box.colEnd + 1
                , HA.style "grid-row-start" <| String.fromInt box.rowStart
                , HA.style "grid-row-end" <| String.fromInt box.rowEnd
                ]
                [ div [] []
                , div [] []
                , div [] []
                ]
            

    in div 
        [ class "game"
        , HA.style "grid-template-rows"
            <| "repeat(" ++ String.fromInt (itemsOnLevel maxLevel) ++ ", 1fr)"
        , HA.style "grid-template-columns"
            <| if maxLevel > 0
                then "repeat(" ++ String.fromInt maxLevel ++ ", 3fr 1fr) 3fr"
                else "3fr"
        ]
        <| List.concatMap
            (\(level, x) ->
                List.concatMap
                    (\(index, phase) ->
                        if level == 0
                        then [ viewSection level index phase ]
                        else [ viewSection level index phase, viewMover level index phase ]
                    )
                <| Dict.toList x
            )
        <| Dict.toList game.levels

update : Msg -> Game comparableId -> Game comparableId
update msg game =
    case msg of
        SetWinner isPlayer2 phase ->
            let
                winnerSlot : PlayerSlot
                winnerSlot = if isPlayer2 then phase.player2 else phase.player1

                looserSlot : PlayerSlot
                looserSlot = Debug.log "looserSlot" <| if isPlayer2 then phase.player1 else phase.player2

                looserId : Maybe comparableId
                looserId = Debug.log "looserId" <| Dict.get looserSlot game.slots

                insertIfGiven : Maybe comparable -> Maybe value -> Dict comparable value -> Dict comparable value
                insertIfGiven key value =
                    case (key, value) of
                        (Just k, Just v) -> Dict.insert k v
                        _ -> identity
            in  { game
                | slots = game.slots
                    |> insertIfGiven phase.winner (Dict.get winnerSlot game.slots)
                    |> insertIfGiven phase.looser (Dict.get looserSlot game.slots)
                , player =
                    if phase.looser /= Nothing
                    then game.player
                    else
                        Maybe.map
                            (\id ->
                                Dict.update id
                                    (Maybe.map (\x -> { x | alive = False }))
                                    game.player
                            )
                            looserId
                        |> Maybe.withDefault game.player
                }
                
