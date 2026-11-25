module LeaderboardTest exposing (suite)

import Expect
import Leaderboard exposing (Msg(..))
import Test exposing (Test, describe, test)


opened : Leaderboard.Model
opened =
    Leaderboard.update (SetOpen True) Leaderboard.init


suite : Test
suite =
    describe "Leaderboard"
        [ describe "observe"
            [ test "the end of the tournament opens it with a celebration" <|
                \_ ->
                    Leaderboard.observe (Just False) (Just True) Leaderboard.init
                        |> Expect.equal { open = True, celebrate = True, celebrations = 1 }
            , test "loading a finished tournament doesn't open it" <|
                \_ ->
                    Leaderboard.observe Nothing (Just True) Leaderboard.init
                        |> Expect.equal Leaderboard.init
            , test "changes of a finished tournament keep it closed" <|
                \_ ->
                    Leaderboard.observe (Just False) (Just True) Leaderboard.init
                        |> Leaderboard.update (SetOpen False)
                        |> Leaderboard.observe (Just True) (Just True)
                        |> .open
                        |> Expect.equal False
            , test "an undo and a new end open it again" <|
                \_ ->
                    Leaderboard.observe (Just False) (Just True) Leaderboard.init
                        |> Leaderboard.update (SetOpen False)
                        |> Leaderboard.observe (Just True) (Just False)
                        |> Leaderboard.observe (Just False) (Just True)
                        |> Expect.equal { open = True, celebrate = True, celebrations = 2 }
            , test "a running tournament doesn't change it" <|
                \_ ->
                    Leaderboard.observe (Just False) (Just False) opened
                        |> Expect.equal opened
            , test "without a tournament it closes" <|
                \_ ->
                    Leaderboard.observe (Just False) Nothing opened
                        |> .open
                        |> Expect.equal False
            ]
        , test "opening it by hand doesn't celebrate" <|
            \_ ->
                opened
                    |> Expect.equal { open = True, celebrate = False, celebrations = 0 }
        ]
