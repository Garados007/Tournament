module Bracket.Knockout.PlayAllTest exposing (suite)

import Bracket.Knockout as Knockout exposing (Game, Msg(..), Places(..), canSetWinner, canUndo, isEliminated, keyedMatches, matchAt, removePlayer)
import Bracket.Knockout.Placement exposing (addPlayer, allPlacements, apply, placements)
import Dict
import Expect exposing (Expectation)
import Helper exposing (inputSlots, knockoutSettings, players, setWinner, start, undo)
import List.Extra
import Placement exposing (Placement)
import Test exposing (Test, describe, test)


settings : Knockout.Settings
settings =
    knockoutSettings PlayAll


startAll : Int -> Game
startAll n =
    apply settings (start n)


counts : List Int
counts =
    List.range 4 17


forEachCount : String -> (Int -> Expectation) -> Test
forEachCount name check =
    describe name <|
        List.map (\n -> test (String.fromInt n ++ " players") <| \_ -> check n) counts


{-| Decides the first open match until none is left. Player 1 wins if `first`
is true for the match.
-}
playOut : (( Int, Int ) -> Bool) -> Game -> Game
playOut first game =
    case List.Extra.find (\( _, match ) -> canSetWinner match game) (keyedMatches game) of
        Just ( key, _ ) ->
            playOut first (setWinner (not <| first key) key game)

        Nothing ->
            game


playOutTop : Game -> Game
playOutTop =
    playOut (always True)


{-| Every place once, each with one player.
-}
expectEveryPlace : List Int -> Game -> Expectation
expectEveryPlace ids game =
    allPlacements game
        |> Expect.all
            [ List.map .place >> Expect.equal (List.range 1 (List.length ids))
            , List.map (.players >> List.length) >> List.all ((==) 1) >> Expect.equal True
            , List.concatMap .players >> List.sort >> Expect.equal ids
            ]


{-| Every slot that a match leads to is the input of exactly one match or a
place, and every place is reached once.
-}
expectLinked : Int -> Game -> Expectation
expectLinked n game =
    let
        sent =
            keyedMatches game
                |> List.concatMap (\( _, match ) -> List.filterMap identity [ match.winner, match.loser ])

        inputs =
            inputSlots game
    in
    Expect.all
        [ \_ ->
            sent
                |> List.filter (\( round, _ ) -> round /= Knockout.placementRound)
                |> List.filter (\slot -> List.length (List.filter ((==) slot) inputs) /= 1)
                |> Expect.equal []
        , \_ ->
            sent
                |> List.filter (\( round, _ ) -> round == Knockout.placementRound)
                |> List.map Tuple.second
                |> List.sort
                |> Expect.equal (List.range 0 (n - 1))
        , \_ ->
            List.Extra.unique sent
                |> List.length
                |> Expect.equal (List.length sent)
        ]
        ()


targets : ( Int, Int ) -> Game -> Maybe ( Maybe ( Int, Int ), Maybe ( Int, Int ) )
targets key game =
    matchAt key game
        |> Maybe.map (\match -> ( match.winner, match.loser ))


{-| The matches of the placement bracket for the places from `first` on.
-}
bracketSize : Int -> Game -> Int
bracketSize first game =
    keyedMatches game
        |> List.filter (\( ( round, _ ), _ ) -> Maybe.map Tuple.first (Knockout.placementBracketOf round) == Just first)
        |> List.length


open : Int -> Placement
open place =
    Placement place []


suite : Test
suite =
    describe "Places: play out all"
        [ forEachCount "every slot is linked and every place reached once" <|
            \n -> expectLinked n (startAll n)
        , forEachCount "every player gets a place" <|
            \n -> expectEveryPlace (players n) (playOutTop (startAll n))
        , forEachCount "every player gets a place when the lower players win" <|
            \n -> expectEveryPlace (players n) (playOut (\( round, index ) -> modBy 2 (round + index) == 0) (startAll n))
        , forEachCount "every player gets a place in a seeded bracket" <|
            \n -> expectEveryPlace (players n) (playOutTop (apply settings (Knockout.startSeeded (players n))))
        , forEachCount "nobody is out" <|
            \n ->
                playOutTop (startAll n)
                    |> (\game -> List.filter (\id -> isEliminated id game) (players n))
                    |> Expect.equal []
        , forEachCount "the banner shows the first three places" <|
            \n ->
                playOutTop (startAll n)
                    |> placements settings
                    |> List.map .place
                    |> Expect.equal [ 1, 2, 3 ]
        , forEachCount "can be applied again" <|
            \n ->
                let
                    game =
                        startAll n |> setWinner False ( Knockout.mainRound n, 0 )
                in
                apply settings game |> Expect.equal game
        , describe "8 players"
            [ test "the losers of the quarter-finals play for the places 5 to 8" <|
                \_ ->
                    List.map (\index -> targets ( 2, index ) (startAll 8) |> Maybe.andThen Tuple.second) [ 0, 1, 2, 3 ]
                        |> Expect.equal (List.map (\index -> Just ( -501, index )) [ 0, 1, 2, 3 ])
            , test "the losers of the semi-finals play for the 3rd place" <|
                \_ ->
                    List.map (\index -> targets ( 1, index ) (startAll 8) |> Maybe.andThen Tuple.second) [ 0, 1 ]
                        |> Expect.equal [ Just ( -300, 0 ), Just ( -300, 1 ) ]
            , test "the places 5 to 8: two matches, then 5th/6th and 7th/8th" <|
                \_ ->
                    startAll 8
                        |> Expect.all
                            [ targets ( -501, 0 ) >> Expect.equal (Just ( Just ( -500, 0 ), Just ( -700, 0 ) ))
                            , targets ( -501, 1 ) >> Expect.equal (Just ( Just ( -500, 1 ), Just ( -700, 1 ) ))
                            , targets ( -500, 0 ) >> Expect.equal (Just ( Just ( -1, 4 ), Just ( -1, 5 ) ))
                            , targets ( -700, 0 ) >> Expect.equal (Just ( Just ( -1, 6 ), Just ( -1, 7 ) ))
                            , targets ( -300, 0 ) >> Expect.equal (Just ( Just ( -1, 2 ), Just ( -1, 3 ) ))
                            , \g -> List.map (\first -> bracketSize first g) [ 3, 5, 7 ] |> Expect.equal [ 1, 3, 1 ]
                            ]
            , test "the loser of a quarter-final moves to the places 5 to 8" <|
                \_ ->
                    startAll 8
                        |> setWinner False ( 2, 0 )
                        |> (.slots >> Dict.get ( -501, 0 ))
                        |> Expect.equal (Just 1)
            ]
        , describe "6 players"
            [ test "the losers of the pre-round play for the places 5 and 6" <|
                \_ ->
                    startAll 6
                        |> Expect.all
                            [ \g -> List.map (\first -> bracketSize first g) [ 3, 5 ] |> Expect.equal [ 1, 1 ]
                            , targets ( -500, 0 ) >> Expect.equal (Just ( Just ( -1, 4 ), Just ( -1, 5 ) ))
                            ]
            ]
        , describe "odd groups get byes"
            [ test "7 players: the three losers of the pre-round play a pre-round and a final" <|
                \_ ->
                    startAll 7
                        |> Expect.all
                            [ \g -> bracketSize 5 g |> Expect.equal 2
                            , targets ( -501, 0 ) >> Expect.equal (Just ( Just ( -500, 0 ), Just ( -1, 6 ) ))
                            ]
            , test "3 players: the loser of the pre-round, P1 against P2, is 3rd" <|
                \_ ->
                    startAll 3
                        |> setWinner False ( 1, 0 )
                        |> placements settings
                        |> Expect.equal [ open 1, open 2, Placement 3 [ 2 ] ]
            ]
        , describe "undo"
            [ test "a quarter-final can't be undone once its loser played on" <|
                \_ ->
                    startAll 8
                        |> setWinner False ( 2, 0 )
                        |> setWinner False ( 2, 1 )
                        |> setWinner False ( -501, 0 )
                        |> (\g -> matchAt ( 2, 0 ) g |> Maybe.map (\match -> canUndo match g))
                        |> Expect.equal (Just False)
            , test "undoing the placement match and then the quarter-final clears the slots" <|
                \_ ->
                    startAll 8
                        |> setWinner False ( 2, 0 )
                        |> setWinner False ( 2, 1 )
                        |> setWinner False ( -501, 0 )
                        |> undo ( -501, 0 )
                        |> undo ( 2, 0 )
                        |> (\g -> List.map (\slot -> Dict.get slot g.slots) [ ( -501, 0 ), ( -501, 1 ), ( -500, 0 ), ( -700, 0 ), ( 1, 0 ) ])
                        |> Expect.equal [ Nothing, Just 3, Nothing, Nothing, Nothing ]
            , test "undoing a final of a placement bracket clears the places" <|
                \_ ->
                    startAll 4
                        |> setWinner False ( 1, 0 )
                        |> setWinner False ( 1, 1 )
                        |> setWinner True ( -300, 0 )
                        |> undo ( -300, 0 )
                        |> allPlacements
                        |> Expect.equal [ open 1, open 2, open 3, open 4 ]
            ]
        , describe "removal"
            [ forEachCount "a removed player gets no place, all others do" <|
                \n ->
                    startAll n
                        |> removePlayer 1
                        |> playOutTop
                        |> allPlacements
                        |> Expect.all
                            [ List.concatMap .players >> List.sort >> Expect.equal (List.filter ((/=) 1) (players n))
                            , List.map .place >> Expect.equal (List.range 1 n)
                            ]
            , test "the removed player loses every placement match by walkover" <|
                \_ ->
                    startAll 8
                        |> removePlayer 1
                        |> playOutTop
                        |> allPlacements
                        |> List.drop 4
                        |> Expect.equal [ Placement 5 [ 3 ], Placement 6 [ 5 ], Placement 7 [ 7 ], open 8 ]
            , test "two removed players meet in a placement bracket" <|
                \_ ->
                    -- P1 moves on by walkover, but gets no place
                    startAll 8
                        |> removePlayer 1
                        |> removePlayer 3
                        |> playOutTop
                        |> allPlacements
                        |> List.drop 4
                        |> Expect.equal [ Placement 5 [ 5 ], open 6, Placement 7 [ 7 ], open 8 ]
            , test "a removed player can't be brought back by undo" <|
                \_ ->
                    startAll 8
                        |> removePlayer 1
                        |> (\g -> matchAt ( -501, 0 ) g |> Maybe.map (\match -> canUndo match g))
                        |> Expect.equal (Just False)
            ]
        , describe "adding players"
            [ forEachCount "keeps every slot linked" <|
                \n ->
                    startAll n
                        |> addPlayer settings n
                        |> expectLinked (n + 1)
            , test "growing from 3 to 4 players moves the 3rd into the match for the 3rd place" <|
                \_ ->
                    startAll 3
                        |> setWinner False ( 1, 0 )
                        |> addPlayer settings 3
                        |> Expect.all
                            [ .slots >> Dict.get ( -1, 2 ) >> Expect.equal Nothing
                            , \g -> List.filterMap (\slot -> Dict.get slot g.slots) [ ( -300, 0 ), ( -300, 1 ) ] |> Expect.equal [ 2 ]
                            , expectLinked 4
                            ]
            , test "growing from 6 to 7 players brings the decided pre-round losers into the bigger placement bracket" <|
                \_ ->
                    startAll 6
                        |> setWinner False ( 2, 0 )
                        |> addPlayer settings 6
                        |> Expect.all
                            [ expectLinked 7
                            , \g -> List.filter (\id -> Knockout.isPlayerUsed id g) [ 1, 6 ] |> Expect.equal [ 1, 6 ]
                            , \g -> playOutTop g |> expectEveryPlace (players 7)
                            ]
            , test "a player can't join if that changes a placement bracket with results" <|
                \_ ->
                    let
                        played =
                            startAll 6
                                |> setWinner False ( 2, 0 )
                                |> setWinner False ( 2, 1 )
                                |> setWinner False ( -500, 0 )
                    in
                    addPlayer settings 6 played
                        |> Expect.all
                            [ \g -> Dict.member 6 g.players |> Expect.equal True
                            , \g -> Knockout.isPlayerUsed 6 g |> Expect.equal False
                            , \g -> g.rounds |> Expect.equal played.rounds
                            ]
            , test "a player can join if the placement brackets with results stay" <|
                \_ ->
                    -- 8 players: a 9th adds a pre-round whose loser is 9th
                    startAll 8
                        |> setWinner False ( 2, 0 )
                        |> setWinner False ( 2, 1 )
                        |> setWinner False ( -501, 0 )
                        |> addPlayer settings 8
                        |> Expect.all
                            [ \g -> Knockout.isPlayerUsed 8 g |> Expect.equal True
                            , \g -> Dict.get ( -500, 0 ) g.slots |> Expect.equal (Just 1)
                            , expectLinked 9
                            ]
            ]
        ]
