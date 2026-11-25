module Bracket.Stepladder exposing
    ( Game
    , Match
    , Msg(..)
    , Settings
    , State
    , Step
    , addPlayer
    , canUndo
    , canWin
    , currentIndex
    , defaultSettings
    , isEliminated
    , isFinished
    , isRemoved
    , matchLength
    , removePlayer
    , start
    , stepName
    , steps
    , toStandings
    , update
    )

{-| The two lowest seeds play first. The winner plays the next higher seed, and
so on, until the final against the top seed.
-}

import Bracket.BestOf as BestOf exposing (BestOf, Games)
import List.Extra
import PlayerId exposing (PlayerId)
import Standings exposing (Row, Standings)


{-| Without `finalBestOf` the final is as long as the other matches.
-}
type alias Settings =
    { bestOf : BestOf
    , finalBestOf : Maybe BestOf
    }


defaultSettings : Settings
defaultSettings =
    { bestOf = 1
    , finalBestOf = Nothing
    }


{-| `walkover` is the winner of a match that ended because the other player
was removed: `True` for player 2.
-}
type alias Match =
    { games : Games
    , walkover : Maybe Bool
    }


{-| `ladder` has the players from the bottom to the top, the top seed last.
Match `i` is played by the winner of match `i - 1` as player 1 (in the first
match the bottom player) and the player at index `i + 1` of the ladder as
player 2. `matches` has the results in this order; a missing entry has no
result yet.

`removed` has every removed player. Players removed before they played aren't
in the ladder anymore.

-}
type alias Game =
    { ladder : List PlayerId
    , matches : List Match
    , removed : List PlayerId
    }


{-| A running stepladder with the settings it was started with.
-}
type alias State =
    { settings : Settings
    , game : Game
    }


{-| The index of the match the message is meant for. It is ignored if the
match can't take it anymore.
-}
type Msg
    = WinGame Bool Int
    | Undo Int


{-| A match of the ladder as it stands. `player1` is only known once the
match before is decided. `winner` is `True` for player 2.
-}
type alias Step =
    { index : Int
    , player1 : Maybe PlayerId
    , player2 : PlayerId
    , match : Match
    , winner : Maybe Bool
    }


emptyMatch : Match
emptyMatch =
    { games = [], walkover = Nothing }


{-| Seeded players come in the order of the list, the top seed first.
-}
start : List PlayerId -> Game
start players =
    { ladder = List.reverse players
    , matches = []
    , removed = []
    }


{-| The last match is the final.
-}
matchLength : Settings -> Game -> Int -> BestOf
matchLength settings game index =
    if index == List.length game.ladder - 2 then
        Maybe.withDefault settings.bestOf settings.finalBestOf

    else
        settings.bestOf


steps : Settings -> Game -> List Step
steps settings game =
    let
        step : PlayerId -> ( List Step, Maybe PlayerId ) -> ( List Step, Maybe PlayerId )
        step player2 ( done, holder ) =
            let
                index : Int
                index =
                    List.length done

                match : Match
                match =
                    List.Extra.getAt index game.matches
                        |> Maybe.withDefault emptyMatch

                winner : Maybe Bool
                winner =
                    case ( holder, match.walkover ) of
                        ( Nothing, _ ) ->
                            Nothing

                        ( Just _, Just isPlayer2 ) ->
                            Just isPlayer2

                        ( Just _, Nothing ) ->
                            BestOf.winner (matchLength settings game index) match.games
            in
            ( { index = index
              , player1 = holder
              , player2 = player2
              , match = match
              , winner = winner
              }
                :: done
            , case winner of
                Just True ->
                    Just player2

                Just False ->
                    holder

                Nothing ->
                    Nothing
            )
    in
    case game.ladder of
        bottom :: rest ->
            List.foldl step ( [], Just bottom ) rest
                |> Tuple.first
                |> List.reverse

        [] ->
            []


winnerOf : Step -> Maybe PlayerId
winnerOf step =
    case step.winner of
        Just True ->
            Just step.player2

        Just False ->
            step.player1

        Nothing ->
            Nothing


loserOf : Step -> Maybe PlayerId
loserOf step =
    case step.winner of
        Just True ->
            step.player1

        Just False ->
            Just step.player2

        Nothing ->
            Nothing


{-| The first match without a winner. `Nothing` once the ladder is finished.
-}
currentIndex : Settings -> Game -> Maybe Int
currentIndex settings game =
    steps settings game
        |> List.Extra.find (.winner >> (==) Nothing)
        |> Maybe.map .index


isFinished : Settings -> Game -> Bool
isFinished settings game =
    currentIndex settings game == Nothing


isRemoved : PlayerId -> Game -> Bool
isRemoved id game =
    List.member id game.removed


{-| Removed players and the losers of decided matches are out.
-}
isEliminated : Settings -> PlayerId -> Game -> Bool
isEliminated settings id game =
    isRemoved id game
        || List.any (\step -> loserOf step == Just id) (steps settings game)


hasResult : Game -> Bool
hasResult game =
    List.any (\match -> match /= emptyMatch) game.matches


{-| The new player joins at the bottom of the ladder, but only until the first
result.
-}
addPlayer : PlayerId -> Game -> Game
addPlayer id game =
    if hasResult game || List.member id game.ladder then
        game

    else
        { game | ladder = id :: game.ladder }


{-| A player who hasn't played yet is taken out of the ladder. If the player is
in the current match, the opponent wins it. Players who are already out keep
their place.
-}
removePlayer : Settings -> PlayerId -> Game -> Game
removePlayer settings id game =
    let
        withRemoved : Game
        withRemoved =
            if isRemoved id game then
                game

            else
                { game | removed = game.removed ++ [ id ] }

        allSteps : List Step
        allSteps =
            steps settings game

        current : Maybe Step
        current =
            List.Extra.find (.winner >> (==) Nothing) allSteps

        hasPlayed : Bool
        hasPlayed =
            List.any
                (\step ->
                    (step.player1 == Just id || step.player2 == id)
                        && (step.winner /= Nothing || not (List.isEmpty step.match.games))
                )
                allSteps
    in
    if not <| List.member id game.ladder then
        withRemoved

    else if not hasPlayed then
        { withRemoved | ladder = List.filter ((/=) id) game.ladder }
            |> trimMatches

    else
        case current of
            Just step ->
                if step.player1 == Just id then
                    setMatch step.index { games = step.match.games, walkover = Just True } withRemoved

                else if step.player2 == id then
                    setMatch step.index { games = step.match.games, walkover = Just False } withRemoved

                else
                    withRemoved

            Nothing ->
                withRemoved


setMatch : Int -> Match -> Game -> Game
setMatch index match game =
    { game
        | matches =
            List.take index game.matches
                ++ List.repeat (index - List.length game.matches) emptyMatch
                ++ match
                :: List.drop (index + 1) game.matches
    }
        |> trimMatches


{-| Matches without a result at the end aren't stored.
-}
trimMatches : Game -> Game
trimMatches game =
    { game
        | matches =
            game.matches
                |> List.reverse
                |> List.Extra.dropWhile ((==) emptyMatch)
                |> List.reverse
    }


{-| Only the current match takes games. Its players are never removed, as a
removal ends the match.
-}
canWin : Settings -> Int -> Game -> Bool
canWin settings index game =
    currentIndex settings game == Just index


{-| The match whose last game can be undone: the current match once it has
games, otherwise the last decided match. A walkover or a match of a removed
player can't be undone, as that would bring the player back.
-}
undoable : Settings -> Game -> Maybe Step
undoable settings game =
    let
        allSteps : List Step
        allSteps =
            steps settings game

        target : Maybe Step
        target =
            case List.Extra.find (.winner >> (==) Nothing) allSteps of
                Just step ->
                    if List.isEmpty step.match.games then
                        List.Extra.getAt (step.index - 1) allSteps

                    else
                        Just step

                Nothing ->
                    List.Extra.last allSteps
    in
    target
        |> Maybe.andThen
            (\step ->
                if
                    (step.match.walkover == Nothing)
                        && not (List.isEmpty step.match.games)
                        && not (List.any (\id -> isRemoved id game) (step.player2 :: List.filterMap identity [ step.player1 ]))
                then
                    Just step

                else
                    Nothing
            )


canUndo : Settings -> Int -> Game -> Bool
canUndo settings index game =
    Maybe.map .index (undoable settings game) == Just index


update : Settings -> Msg -> Game -> Game
update settings msg game =
    case msg of
        WinGame isPlayer2 index ->
            if canWin settings index game then
                let
                    match : Match
                    match =
                        List.Extra.getAt index game.matches
                            |> Maybe.withDefault emptyMatch
                in
                setMatch index
                    { match | games = BestOf.addGame (matchLength settings game index) isPlayer2 match.games }
                    game

            else
                game

        Undo index ->
            case undoable settings game of
                Just step ->
                    if step.index == index then
                        setMatch index { games = BestOf.removeLastGame step.match.games, walkover = Nothing } game

                    else
                        game

                Nothing ->
                    game


{-| The winner of the final, or the only player of a ladder without matches.
-}
championOf : List Step -> Game -> Maybe PlayerId
championOf allSteps game =
    case allSteps of
        [] ->
            List.head game.ladder

        _ ->
            List.Extra.last allSteps |> Maybe.andThen winnerOf


{-| "Final" for the last match, otherwise "Step 1" and so on.
-}
stepName : Game -> Int -> String
stepName game index =
    if index == List.length game.ladder - 2 then
        "Final"

    else
        "Step " ++ String.fromInt (index + 1)


{-| One place per step: the loser of a match gets the place of the players
below. A walkover counts as a win or a loss, but a removed player gets no place
for it.
-}
toStandings : Settings -> Game -> Standings
toStandings settings game =
    let
        allSteps : List Step
        allSteps =
            steps settings game

        place : PlayerId -> Maybe Int
        place id =
            if championOf allSteps game == Just id then
                Just 1

            else
                allSteps
                    |> List.Extra.find (\step -> loserOf step == Just id)
                    |> Maybe.andThen
                        (\step ->
                            if step.match.walkover /= Nothing && isRemoved id game then
                                Nothing

                            else
                                Just <| List.length game.ladder - step.index
                        )

        reached : PlayerId -> Maybe Int
        reached id =
            allSteps
                |> List.filter (\step -> step.player1 == Just id || step.player2 == id)
                |> List.map .index
                |> List.maximum

        count : (Step -> Maybe PlayerId) -> PlayerId -> Int
        count side id =
            List.length <| List.filter (\step -> side step == Just id) allSteps

        toRow : PlayerId -> Row
        toRow id =
            let
                base : Row
                base =
                    Standings.row
                        { id = id
                        , place = place id
                        , alive = not (isEliminated settings id game)
                        , removed = isRemoved id game
                        }
            in
            { base
                | won = count winnerOf id
                , lost = count loserOf id
                , extra = [ reached id |> Maybe.map (stepName game) |> Maybe.withDefault "" ]
            }
    in
    { rows =
        (game.ladder ++ List.filter (\id -> not <| List.member id game.ladder) game.removed)
            |> List.map toRow
            |> Standings.order (reached >> Maybe.withDefault -1 >> negate)
    , finished = isFinished settings game
    , liveRanking = False
    , draws = False
    , columns = [ "Step" ]
    }
