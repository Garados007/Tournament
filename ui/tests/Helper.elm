module Helper exposing
    ( addPlayer
    , allPhases
    , expectValidStructure
    , inputSlots
    , matchesPerLevel
    , phaseAt
    , players
    , setWinner
    , start
    , startWithPlacement
    )

import Bracket.Knockout exposing (Game, Msg(..), Phase, Player, PlayerSlot)
import Bracket.Knockout.Placement
import Dict
import Expect exposing (Expectation)


players : Int -> List ( Int, String )
players n =
    List.range 0 (n - 1)
        |> List.map (\i -> ( i, "P" ++ String.fromInt i ))


start : Int -> Game Int
start n =
    Bracket.Knockout.generateTournament n
        |> Bracket.Knockout.setInitialPlayer (players n)


{-| Starts a tournament the way `Main.update StartGame` does.
-}
startWithPlacement : Int -> Game Int
startWithPlacement n =
    start n
        |> Bracket.Knockout.Placement.getPlacement
        |> Tuple.first


{-| Adds a player the way `Main.update AddName` does: register the player, then
add a position for it.
-}
addPlayer : Int -> Game Int -> Maybe (Game Int)
addPlayer id game =
    Bracket.Knockout.addPosition
        { game
            | player =
                Dict.insert id
                    (Player id ("P" ++ String.fromInt id) True)
                    game.player
        }


allPhases : Game id -> List ( ( Int, Int ), Phase )
allPhases game =
    Dict.toList game.levels
        |> List.concatMap
            (\( level, phases ) ->
                Dict.toList phases
                    |> List.map (Tuple.mapFirst (Tuple.pair level))
            )


phaseAt : ( Int, Int ) -> Game id -> Maybe Phase
phaseAt ( level, index ) game =
    Dict.get level game.levels
        |> Maybe.andThen (Dict.get index)


{-| Clicks the winner button of player 1 (`False`) or player 2 (`True`) of the
match at the given position. A missing match leaves the game unchanged.
-}
setWinner : Bool -> ( Int, Int ) -> Game comparable -> Game comparable
setWinner isPlayer2 pos game =
    case phaseAt pos game of
        Just phase ->
            Bracket.Knockout.update (SetWinner isPlayer2 phase) game

        Nothing ->
            game


inputSlots : Game id -> List PlayerSlot
inputSlots game =
    allPhases game
        |> List.concatMap (\( _, phase ) -> [ phase.player1, phase.player2 ])


matchesPerLevel : Game id -> List Int
matchesPerLevel game =
    Dict.values game.levels
        |> List.map Dict.size


{-| Every input slot belongs to exactly one match, and every winner or loser
slot with a level of 0 or more is the input of another match. Negative levels
are the placement slots.
-}
expectValidStructure : Game id -> Expectation
expectValidStructure game =
    let
        inputs =
            inputSlots game

        targets =
            allPhases game
                |> List.concatMap
                    (\( _, phase ) -> List.filterMap identity [ phase.winner, phase.looser ])
                |> List.filter (\( level, _ ) -> level >= 0)

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
