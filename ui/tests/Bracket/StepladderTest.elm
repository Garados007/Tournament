module Bracket.StepladderTest exposing (suite)

import Bracket.Stepladder as Stepladder exposing (Game, Msg(..), Settings)
import Bracket.Stepladder.Serializer as Serializer
import Expect
import Json.Decode as JD
import Standings
import Test exposing (Test, describe, test)


single : Settings
single =
    Stepladder.defaultSettings


{-| Seeds 0 (top) to 4 (bottom).
-}
five : Game
five =
    Stepladder.start [ 0, 1, 2, 3, 4 ]


win : Bool -> Int -> Game -> Game
win isPlayer2 index =
    Stepladder.update single (WinGame isPlayer2 index)


undo : Int -> Game -> Game
undo index =
    Stepladder.update single (Undo index)


remove : Int -> Game -> Game
remove =
    Stepladder.removePlayer single


players : Game -> List ( Maybe Int, Int )
players game =
    Stepladder.steps single game
        |> List.map (\step -> ( step.player1, step.player2 ))


current : Game -> Maybe Int
current =
    Stepladder.currentIndex single


podium : Game -> List ( Int, List Int )
podium =
    Stepladder.toStandings single >> Standings.podium


eliminated : List Int -> Game -> List Bool
eliminated ids game =
    List.map (\id -> Stepladder.isEliminated single id game) ids


{-| The higher seed wins every match.
-}
seedsWin : Game -> Game
seedsWin =
    win True 0 >> win True 1 >> win True 2 >> win True 3


suite : Test
suite =
    describe "Bracket.Stepladder"
        [ describe "start"
            [ test "the first player is at the top of the ladder" <|
                \_ ->
                    five.ladder
                        |> Expect.equal [ 4, 3, 2, 1, 0 ]
            , test "the two lowest seeds play first, the top seed waits for the final" <|
                \_ ->
                    players five
                        |> Expect.equal [ ( Just 4, 3 ), ( Nothing, 2 ), ( Nothing, 1 ), ( Nothing, 0 ) ]
            , test "the first match is the current one" <|
                \_ ->
                    current five
                        |> Expect.equal (Just 0)
            , test "no place is decided yet" <|
                \_ ->
                    podium five
                        |> Expect.equal []
            ]
        , describe "playing"
            [ test "the winner plays the next higher seed" <|
                \_ ->
                    win False 0 five
                        |> players
                        |> Expect.equal [ ( Just 4, 3 ), ( Just 4, 2 ), ( Nothing, 1 ), ( Nothing, 0 ) ]
            , test "only the current match takes games" <|
                \_ ->
                    win False 1 five
                        |> Expect.equal five
            , test "a decided match takes no more games" <|
                \_ ->
                    win False 0 five
                        |> (\game -> win True 0 game |> Expect.equal game)
            , test "the loser of the first match is out, the winner isn't" <|
                \_ ->
                    win False 0 five
                        |> eliminated [ 4, 3 ]
                        |> Expect.equal [ False, True ]
            , test "the ladder is finished after the final" <|
                \_ ->
                    seedsWin five
                        |> Stepladder.isFinished single
                        |> Expect.equal True
            ]
        , describe "places"
            [ test "the winner of the final is 1st, its loser 2nd, the loser of the step before 3rd" <|
                \_ ->
                    seedsWin five
                        |> podium
                        |> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 1 ] ), ( 3, [ 2 ] ) ]
            , test "a player who climbs the whole ladder wins" <|
                \_ ->
                    five
                        |> win False 0
                        |> win False 1
                        |> win False 2
                        |> win False 3
                        |> podium
                        |> Expect.equal [ ( 1, [ 4 ] ), ( 2, [ 0 ] ), ( 3, [ 1 ] ) ]
            , test "the 3rd place is decided before the final" <|
                \_ ->
                    five
                        |> win True 0
                        |> win True 1
                        |> win False 2
                        |> podium
                        |> Expect.equal [ ( 3, [ 1 ] ) ]
            , test "two players only have two places" <|
                \_ ->
                    Stepladder.start [ 0, 1 ]
                        |> win False 0
                        |> podium
                        |> Expect.equal [ ( 1, [ 1 ] ), ( 2, [ 0 ] ) ]
            ]
        , describe "best of"
            [ test "the final has its own length" <|
                \_ ->
                    let
                        settings =
                            { bestOf = 1, finalBestOf = Just 3 }

                        game =
                            Stepladder.start [ 0, 1, 2 ]
                                |> Stepladder.update settings (WinGame True 0)
                                |> Stepladder.update settings (WinGame True 1)
                    in
                    ( Stepladder.matchLength settings game 0
                    , Stepladder.matchLength settings game 1
                    , Stepladder.currentIndex settings game
                    )
                        |> Expect.equal ( 1, 3, Just 1 )
            , test "a match is decided once a player has won enough games" <|
                \_ ->
                    let
                        settings =
                            { bestOf = 3, finalBestOf = Nothing }

                        play isPlayer2 =
                            Stepladder.update settings (WinGame isPlayer2 0)
                    in
                    Stepladder.start [ 0, 1, 2 ]
                        |> play True
                        |> play False
                        |> play True
                        |> Stepladder.currentIndex settings
                        |> Expect.equal (Just 1)
            ]
        , describe "adding a player"
            [ test "joins at the bottom before the first result" <|
                \_ ->
                    Stepladder.addPlayer 5 five
                        |> players
                        |> List.head
                        |> Expect.equal (Just ( Just 5, 4 ))
            , test "is not possible after the first result" <|
                \_ ->
                    let
                        game =
                            win False 0 five
                    in
                    Stepladder.addPlayer 5 game
                        |> Expect.equal game
            , test "a player already in the ladder isn't added again" <|
                \_ ->
                    Stepladder.addPlayer 4 five
                        |> Expect.equal five
            ]
        , describe "removing a player before they played"
            [ test "takes a waiting player out of the ladder" <|
                \_ ->
                    win False 0 five
                        |> remove 1
                        |> players
                        |> Expect.equal [ ( Just 4, 3 ), ( Just 4, 2 ), ( Nothing, 0 ) ]
            , test "gives them no place and moves the places of the others" <|
                \_ ->
                    five
                        |> win True 0
                        |> remove 1
                        |> win True 1
                        |> win True 2
                        |> podium
                        |> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 2 ] ), ( 3, [ 3 ] ) ]
            , test "marks them as out" <|
                \_ ->
                    remove 1 five
                        |> eliminated [ 1 ]
                        |> Expect.equal [ True ]
            , test "takes the bottom player out before the first game" <|
                \_ ->
                    remove 4 five
                        |> players
                        |> Expect.equal [ ( Just 3, 2 ), ( Nothing, 1 ), ( Nothing, 0 ) ]
            , test "takes the opponent of the current match out before the first game" <|
                \_ ->
                    win False 0 five
                        |> remove 2
                        |> players
                        |> Expect.equal [ ( Just 4, 3 ), ( Just 4, 1 ), ( Nothing, 0 ) ]
            , test "leaves the last player as the winner" <|
                \_ ->
                    Stepladder.start [ 0, 1 ]
                        |> remove 1
                        |> podium
                        |> Expect.equal [ ( 1, [ 0 ] ) ]
            ]
        , describe "removing a player in the current match"
            [ test "lets the opponent win it" <|
                \_ ->
                    win False 0 five
                        |> remove 4
                        |> players
                        |> Expect.equal [ ( Just 4, 3 ), ( Just 4, 2 ), ( Just 2, 1 ), ( Nothing, 0 ) ]
            , test "lets the climber win if the higher seed already played a game" <|
                \_ ->
                    let
                        settings =
                            { bestOf = 3, finalBestOf = Nothing }
                    in
                    five
                        |> Stepladder.update settings (WinGame True 0)
                        |> Stepladder.removePlayer settings 3
                        |> Stepladder.steps settings
                        |> List.map .winner
                        |> Expect.equal [ Just False, Nothing, Nothing, Nothing ]
            , test "gives them no place" <|
                \_ ->
                    five
                        |> win True 0
                        |> win True 1
                        |> win True 2
                        |> remove 1
                        |> podium
                        |> Expect.equal [ ( 1, [ 0 ] ), ( 3, [ 2 ] ) ]
            , test "the walkover can't be undone" <|
                \_ ->
                    win False 0 five
                        |> remove 4
                        |> (\game -> Stepladder.canUndo single 1 game || Stepladder.canUndo single 0 game)
                        |> Expect.equal False
            ]
        , describe "removing a player who is already out"
            [ test "keeps their place" <|
                \_ ->
                    seedsWin five
                        |> remove 2
                        |> podium
                        |> Expect.equal (podium <| seedsWin five)
            , test "marks them as removed" <|
                \_ ->
                    seedsWin five
                        |> remove 2
                        |> Stepladder.isRemoved 2
                        |> Expect.equal True
            ]
        , describe "undo"
            [ test "brings the last decided match back" <|
                \_ ->
                    five
                        |> win True 0
                        |> win False 1
                        |> undo 1
                        |> Expect.equal (win True 0 five)
            , test "is only possible for the last decided match" <|
                \_ ->
                    five
                        |> win True 0
                        |> win False 1
                        |> (\game -> ( Stepladder.canUndo single 0 game, undo 0 game == game ))
                        |> Expect.equal ( False, True )
            , test "undoes the final" <|
                \_ ->
                    seedsWin five
                        |> undo 3
                        |> current
                        |> Expect.equal (Just 3)
            , test "removes the last game of the current match first" <|
                \_ ->
                    let
                        settings =
                            { bestOf = 3, finalBestOf = Nothing }

                        game =
                            Stepladder.start [ 0, 1, 2 ]
                                |> Stepladder.update settings (WinGame True 0)
                                |> Stepladder.update settings (WinGame True 0)
                                |> Stepladder.update settings (WinGame False 1)
                    in
                    ( Stepladder.canUndo settings 0 game
                    , Stepladder.update settings (Undo 1) game
                    )
                        |> Expect.equal
                            ( False
                            , Stepladder.start [ 0, 1, 2 ]
                                |> Stepladder.update settings (WinGame True 0)
                                |> Stepladder.update settings (WinGame True 0)
                            )
            , test "is not possible without a result" <|
                \_ ->
                    Stepladder.canUndo single 0 five
                        |> Expect.equal False
            , test "is not possible for a match of a removed player" <|
                \_ ->
                    seedsWin five
                        |> remove 1
                        |> Stepladder.canUndo single 3
                        |> Expect.equal False
            ]
        , test "the serializer keeps the game" <|
            \_ ->
                five
                    |> win True 0
                    |> win False 1
                    |> remove 0
                    |> (\game ->
                            Serializer.encodeGame game
                                |> JD.decodeValue Serializer.decodeGame
                                |> Expect.equal (Ok game)
                       )
        ]
