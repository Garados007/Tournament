module Bracket.RoundRobin.CsvTest exposing (suite)

import Bracket.RoundRobin as RR exposing (Matches(..), Msg(..), Ranking(..), Scoring(..), Settings, State)
import Dict exposing (Dict)
import Expect
import PlayerId exposing (PlayerId)
import Test exposing (Test, describe, test)
import Tournament exposing (Tournament(..))


names : Dict PlayerId String
names =
    Dict.fromList [ ( 0, "A" ), ( 1, "B" ), ( 2, "C" ), ( 3, "D;x" ) ]


{-| A beats B, draws with C and beats D. D is removed, so B and C win their
matches against D. B and C haven't played yet.
-}
tournament : Settings -> State
tournament settings =
    RR.start settings [ 0, 1, 2, 3 ]
        |> RR.update (SetResult 0 1 ( 3, 1 ))
        |> RR.update (SetResult 2 0 ( 2, 2 ))
        |> RR.update (SetResult 3 0 ( 0, 1 ))
        |> RR.update (SetResult 1 2 ( 0, 1 ))
        |> RR.update (ClearResult 1 2)
        |> RR.removePlayer 3


csv : Settings -> Maybe String
csv settings =
    Tournament.csv (RoundRobin <| tournament settings)
        |> Maybe.map (\toCsv -> toCsv names)


lines : List String -> String
lines =
    List.map (\line -> line ++ "\u{000D}\n") >> String.concat >> (++) "\u{FEFF}"


suite : Test
suite =
    describe "Round robin CSV"
        [ test "with points, ranked by results" <|
            \_ ->
                csv { matches = Single, scoring = Points, ranking = ByResults }
                    |> Expect.equal
                        (Just <|
                            lines
                                [ ";A;B;C;\"D;x\";W;D;L;Score;Pts;Place"
                                , "A;;3 : 1;2 : 2;1 : 0;2;1;0;6 : 3;7;1"
                                , "B;1 : 3;;;w/o;1;0;1;1 : 3;3;3"
                                , "C;2 : 2;;;w/o;1;1;0;2 : 2;4;2"
                                , "\"D;x\";0 : 1;w/o;w/o;;0;0;3;0 : 1;0;"
                                ]
                        )
        , test "with points, ranked by total points" <|
            \_ ->
                csv { matches = Single, scoring = Points, ranking = ByTotalPoints }
                    |> Expect.equal
                        (Just <|
                            lines
                                [ ";A;B;C;\"D;x\";W;D;L;Score;Place"
                                , "A;;3 : 1;2 : 2;1 : 0;2;1;0;6 : 3;1"
                                , "B;1 : 3;;;w/o;1;0;1;1 : 3;3"
                                , "C;2 : 2;;;w/o;1;1;0;2 : 2;2"
                                , "\"D;x\";0 : 1;w/o;w/o;;0;0;3;0 : 1;"
                                ]
                        )
        , test "with the winner only" <|
            \_ ->
                csv { matches = Single, scoring = WinnerOnly, ranking = ByTotalPoints }
                    |> Expect.equal
                        (Just <|
                            lines
                                [ ";A;B;C;\"D;x\";W;D;L;Pts;Place"
                                , "A;;W;D;W;2;1;0;7;1"
                                , "B;L;;;w/o;1;0;1;3;3"
                                , "C;D;;;w/o;1;1;0;4;2"
                                , "\"D;x\";L;w/o;w/o;;0;0;3;0;"
                                ]
                        )
        , test "a knockout offers no CSV" <|
            \_ ->
                Tournament.start Tournament.defaultSettings [ 0, 1 ]
                    |> Tournament.csv
                    |> Maybe.map (always ())
                    |> Expect.equal Nothing
        ]
