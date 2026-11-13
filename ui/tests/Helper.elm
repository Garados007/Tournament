module Helper exposing
    ( addPlayer
    , allMatches
    , expectValidStructure
    , grow
    , inputSlots
    , matchAt
    , matchesPerRound
    , players
    , setWinner
    , start
    , startWith
    , startWithPlacement
    , undo
    )

import Bracket.Knockout exposing (Game, Match, Msg(..), Player, PlayerSlot, ThirdPlace(..))
import Bracket.Knockout.Placement
import Dict
import Expect exposing (Expectation)


players : Int -> List Int
players n =
    List.range 0 (n - 1)


start : Int -> Game
start n =
    Bracket.Knockout.start (players n)


{-| Starts a tournament the way `Tournament.start` does.
-}
startWith : ThirdPlace -> Int -> Game
startWith thirdPlace n =
    start n
        |> Bracket.Knockout.Placement.apply { thirdPlace = thirdPlace }


startWithPlacement : Int -> Game
startWithPlacement =
    startWith Playoff


{-| Adds a player the way `Tournament.addPlayer` does.
-}
grow : ThirdPlace -> Int -> Game -> Game
grow thirdPlace id =
    Bracket.Knockout.addPlayer id
        >> Bracket.Knockout.Placement.apply { thirdPlace = thirdPlace }


{-| Like `Bracket.Knockout.addPlayer`, but `Nothing` if there is no free
position.
-}
addPlayer : Int -> Game -> Maybe Game
addPlayer id game =
    Bracket.Knockout.addPosition
        { game
            | players =
                Dict.insert id
                    (Player id True False)
                    game.players
        }


allMatches : Game -> List ( ( Int, Int ), Match )
allMatches game =
    Dict.toList game.rounds
        |> List.concatMap
            (\( round, matches ) ->
                Dict.toList matches
                    |> List.map (Tuple.mapFirst (Tuple.pair round))
            )


matchAt : ( Int, Int ) -> Game -> Maybe Match
matchAt ( round, index ) game =
    Dict.get round game.rounds
        |> Maybe.andThen (Dict.get index)


{-| Clicks the winner button of player 1 (`False`) or player 2 (`True`) of the
match at the given position. A missing match leaves the game unchanged.
-}
setWinner : Bool -> ( Int, Int ) -> Game -> Game
setWinner isPlayer2 pos game =
    case matchAt pos game of
        Just match ->
            Bracket.Knockout.update (SetWinner isPlayer2 match) game

        Nothing ->
            game


{-| Clicks the undo button of the match at the given position.
-}
undo : ( Int, Int ) -> Game -> Game
undo pos game =
    case matchAt pos game of
        Just match ->
            Bracket.Knockout.update (Undo match) game

        Nothing ->
            game


inputSlots : Game -> List PlayerSlot
inputSlots game =
    allMatches game
        |> List.concatMap (\( _, match ) -> [ match.player1, match.player2 ])


matchesPerRound : Game -> List Int
matchesPerRound game =
    Dict.values game.rounds
        |> List.map Dict.size


{-| Every input slot belongs to exactly one match, and every winner or loser
slot with a round of 0 or more is the input of another match. Negative rounds
are the placement slots.
-}
expectValidStructure : Game -> Expectation
expectValidStructure game =
    let
        inputs =
            inputSlots game

        targets =
            allMatches game
                |> List.concatMap
                    (\( _, match ) -> List.filterMap identity [ match.winner, match.loser ])
                |> List.filter (\( round, _ ) -> round >= 0)

        duplicates list =
            List.filter (\x -> List.length (List.filter ((==) x) list) > 1) list
    in
    Expect.all
        [ \_ ->
            duplicates inputs
                |> Expect.equal []
                |> Expect.onFail "input slots are used by more than one match"
        , \_ ->
            List.filter (\t -> not <| List.member t inputs) targets
                |> Expect.equal []
                |> Expect.onFail "winner or loser slots point to no match input"
        , \_ ->
            duplicates targets
                |> Expect.equal []
                |> Expect.onFail "two matches send a player to the same slot"
        ]
        ()
