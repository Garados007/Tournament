module Bracket.RoundRobinTest exposing (suite)

import Bracket.RoundRobin as RR exposing (Cell(..), Matches(..), Msg(..), Ranking(..), Scoring(..), Settings, State)
import Bracket.RoundRobin.Serializer as Serializer
import Expect
import Json.Decode as JD
import Json.Encode as JE
import Placement exposing (Placement)
import Test exposing (Test, describe, test)


settings : Matches -> Scoring -> Ranking -> Settings
settings matches scoring ranking =
    { matches = matches, scoring = scoring, ranking = ranking }


points : Settings
points =
    settings Single Points ByResults


winnerOnly : Settings
winnerOnly =
    settings Single WinnerOnly ByResults


totalPoints : Settings
totalPoints =
    settings Single Points ByTotalPoints


{-| Starts with the players 0 to n-1 and enters the results as (row, column,
scores).
-}
play : Settings -> Int -> List ( Int, Int, ( Int, Int ) ) -> State
play s n results =
    List.foldl
        (\( row, column, score ) -> RR.update (SetResult row column score))
        (RR.start s (List.range 0 (n - 1)))
        results


places : State -> List ( Int, Maybe Int )
places state =
    RR.standings state
        |> List.map (\standing -> ( standing.id, standing.place ))


type alias RankingCase =
    { name : String
    , settings : Settings
    , players : Int
    , results : List ( Int, Int, ( Int, Int ) )
    , expected : List ( Int, Maybe Int )
    }


rankingCases : List RankingCase
rankingCases =
    [ { name = "match points decide"
      , settings = points
      , players = 3
      , results = [ ( 0, 1, ( 2, 1 ) ), ( 0, 2, ( 2, 1 ) ), ( 1, 2, ( 1, 0 ) ) ]
      , expected = [ ( 0, Just 1 ), ( 1, Just 2 ), ( 2, Just 3 ) ]
      }
    , { name = "the score difference decides equal match points"
      , settings = points
      , players = 3
      , results = [ ( 0, 1, ( 5, 0 ) ), ( 1, 2, ( 1, 0 ) ), ( 2, 0, ( 1, 0 ) ) ]
      , expected = [ ( 0, Just 1 ), ( 2, Just 2 ), ( 1, Just 3 ) ]
      }
    , { name = "the points scored decide an equal score difference"
      , settings = points
      , players = 3
      , results = [ ( 0, 1, ( 4, 3 ) ), ( 1, 2, ( 1, 0 ) ), ( 2, 0, ( 2, 1 ) ) ]
      , expected = [ ( 0, Just 1 ), ( 1, Just 2 ), ( 2, Just 3 ) ]
      }
    , { name = "the head-to-head decides equal points"
      , settings = points
      , players = 4
      , results =
            [ ( 0, 1, ( 1, 0 ) )
            , ( 0, 2, ( 0, 1 ) )
            , ( 1, 2, ( 1, 0 ) )
            , ( 0, 3, ( 1, 1 ) )
            , ( 1, 3, ( 1, 1 ) )
            , ( 2, 3, ( 3, 0 ) )
            ]
      , expected = [ ( 2, Just 1 ), ( 0, Just 2 ), ( 1, Just 3 ), ( 3, Just 4 ) ]
      }
    , { name = "the number of wins decides an equal head-to-head"
      , settings = winnerOnly
      , players = 5
      , results =
            [ ( 0, 1, ( 0, 0 ) )
            , ( 0, 2, ( 1, 0 ) )
            , ( 0, 3, ( 0, 1 ) )
            , ( 0, 4, ( 0, 1 ) )
            , ( 1, 2, ( 0, 0 ) )
            , ( 1, 3, ( 0, 0 ) )
            , ( 1, 4, ( 0, 0 ) )
            , ( 2, 3, ( 0, 0 ) )
            , ( 2, 4, ( 0, 0 ) )
            , ( 3, 4, ( 0, 0 ) )
            ]
      , expected = [ ( 3, Just 1 ), ( 4, Just 1 ), ( 0, Just 3 ), ( 1, Just 4 ), ( 2, Just 5 ) ]
      }
    , { name = "tied players share a place and the next place is skipped"
      , settings = points
      , players = 4
      , results =
            [ ( 0, 1, ( 1, 0 ) )
            , ( 0, 2, ( 1, 0 ) )
            , ( 0, 3, ( 1, 0 ) )
            , ( 1, 2, ( 0, 0 ) )
            , ( 1, 3, ( 1, 0 ) )
            , ( 2, 3, ( 1, 0 ) )
            ]
      , expected = [ ( 0, Just 1 ), ( 1, Just 2 ), ( 2, Just 2 ), ( 3, Just 4 ) ]
      }
    , { name = "the head-to-head among 3 tied players"
      , settings = winnerOnly
      , players = 5
      , results =
            [ ( 0, 1, ( 1, 0 ) )
            , ( 0, 2, ( 1, 0 ) )
            , ( 1, 2, ( 1, 0 ) )
            , ( 0, 3, ( 0, 1 ) )
            , ( 0, 4, ( 0, 1 ) )
            , ( 1, 3, ( 1, 0 ) )
            , ( 1, 4, ( 0, 1 ) )
            , ( 2, 3, ( 1, 0 ) )
            , ( 2, 4, ( 1, 0 ) )
            , ( 3, 4, ( 0, 0 ) )
            ]
      , expected = [ ( 4, Just 1 ), ( 0, Just 2 ), ( 1, Just 3 ), ( 2, Just 4 ), ( 3, Just 5 ) ]
      }
    , { name = "a circle of wins is a shared place"
      , settings = winnerOnly
      , players = 3
      , results = [ ( 0, 1, ( 1, 0 ) ), ( 1, 2, ( 1, 0 ) ), ( 2, 0, ( 1, 0 ) ) ]
      , expected = [ ( 0, Just 1 ), ( 1, Just 1 ), ( 2, Just 1 ) ]
      }
    , { name = "a draw with winner only is a shared place"
      , settings = winnerOnly
      , players = 2
      , results = [ ( 0, 1, ( 0, 0 ) ) ]
      , expected = [ ( 0, Just 1 ), ( 1, Just 1 ) ]
      }
    , { name = "winner only ignores the scores"
      , settings = winnerOnly
      , players = 3
      , results = [ ( 0, 1, ( 9, 0 ) ), ( 1, 2, ( 1, 0 ) ), ( 2, 0, ( 1, 0 ) ) ]
      , expected = [ ( 0, Just 1 ), ( 1, Just 1 ), ( 2, Just 1 ) ]
      }
    , { name = "winner only ignores the total points ranking"
      , settings = settings Single WinnerOnly ByTotalPoints
      , players = 2
      , results = [ ( 0, 1, ( 0, 1 ) ) ]
      , expected = [ ( 1, Just 1 ), ( 0, Just 2 ) ]
      }
    , { name = "total points: the points scored decide"
      , settings = totalPoints
      , players = 3
      , results = [ ( 0, 1, ( 0, 5 ) ), ( 0, 2, ( 9, 0 ) ), ( 1, 2, ( 0, 1 ) ) ]
      , expected = [ ( 0, Just 1 ), ( 1, Just 2 ), ( 2, Just 3 ) ]
      }
    , { name = "total points: fewer points conceded decide"
      , settings = totalPoints
      , players = 3
      , results = [ ( 0, 1, ( 2, 2 ) ), ( 0, 2, ( 2, 0 ) ), ( 1, 2, ( 2, 5 ) ) ]
      , expected = [ ( 2, Just 1 ), ( 0, Just 2 ), ( 1, Just 3 ) ]
      }
    , { name = "total points: the head-to-head decides"
      , settings = totalPoints
      , players = 4
      , results =
            [ ( 0, 1, ( 1, 0 ) )
            , ( 0, 2, ( 0, 0 ) )
            , ( 0, 3, ( 0, 1 ) )
            , ( 1, 2, ( 1, 0 ) )
            , ( 1, 3, ( 0, 0 ) )
            , ( 2, 3, ( 5, 5 ) )
            ]
      , expected = [ ( 3, Just 1 ), ( 2, Just 2 ), ( 0, Just 3 ), ( 1, Just 4 ) ]
      }
    , { name = "total points: still equal is a shared place"
      , settings = totalPoints
      , players = 3
      , results = [ ( 0, 1, ( 1, 0 ) ), ( 0, 2, ( 0, 1 ) ), ( 1, 2, ( 1, 0 ) ) ]
      , expected = [ ( 0, Just 1 ), ( 1, Just 1 ), ( 2, Just 1 ) ]
      }
    , { name = "rematch counts both matches"
      , settings = settings Rematch Points ByResults
      , players = 2
      , results = [ ( 0, 1, ( 1, 0 ) ), ( 1, 0, ( 2, 0 ) ) ]
      , expected = [ ( 1, Just 1 ), ( 0, Just 2 ) ]
      }
    ]


cellsOf01 : State -> ( Cell, Cell )
cellsOf01 state =
    ( RR.cell 0 1 state, RR.cell 1 0 state )


{-| 3 players, player 0 beat player 1 and player 2 left before playing.
-}
withRemoved : State
withRemoved =
    play points 3 [ ( 0, 1, ( 2, 1 ) ), ( 2, 0, ( 4, 0 ) ) ]
        |> RR.removePlayer 2


roundTrip : State -> Expect.Expectation
roundTrip state =
    ( Serializer.encodeSettings state.settings, Serializer.encodeGame state.game )
        |> Tuple.mapBoth (JE.encode 0) (JE.encode 0)
        |> (\( s, g ) ->
                Result.map2 State
                    (JD.decodeString Serializer.decodeSettings s)
                    (JD.decodeString Serializer.decodeGame g)
           )
        |> Expect.equal (Ok state)


suite : Test
suite =
    describe "Bracket.RoundRobin"
        [ describe "ranking" <|
            List.map
                (\case_ ->
                    test case_.name <|
                        \_ ->
                            play case_.settings case_.players case_.results
                                |> places
                                |> Expect.equal case_.expected
                )
                rankingCases
        , describe "results"
            [ test "a single match shows mirrored in both cells" <|
                \_ ->
                    play points 2 [ ( 1, 0, ( 3, 2 ) ) ]
                        |> cellsOf01
                        |> Expect.equal ( Played ( 2, 3 ), Played ( 3, 2 ) )
            , test "entering the mirrored cell replaces a single match" <|
                \_ ->
                    play points 2 [ ( 1, 0, ( 3, 2 ) ), ( 0, 1, ( 5, 5 ) ) ]
                        |> cellsOf01
                        |> Expect.equal ( Played ( 5, 5 ), Played ( 5, 5 ) )
            , test "rematches are independent" <|
                \_ ->
                    play (settings Rematch Points ByResults) 2 [ ( 0, 1, ( 3, 2 ) ) ]
                        |> cellsOf01
                        |> Expect.equal ( Played ( 3, 2 ), Open )
            , test "both rematches can have results" <|
                \_ ->
                    play (settings Rematch Points ByResults) 2 [ ( 0, 1, ( 3, 2 ) ), ( 1, 0, ( -1, 1 ) ) ]
                        |> cellsOf01
                        |> Expect.equal ( Played ( 3, 2 ), Played ( -1, 1 ) )
            , test "a result can be cleared" <|
                \_ ->
                    play points 2 [ ( 0, 1, ( 3, 2 ) ) ]
                        |> RR.update (ClearResult 1 0)
                        |> cellsOf01
                        |> Expect.equal ( Open, Open )
            , test "winner only stores who won" <|
                \_ ->
                    play winnerOnly 2 [ ( 0, 1, ( 5, 2 ) ) ]
                        |> cellsOf01
                        |> Expect.equal ( Played ( 1, 0 ), Played ( 0, 1 ) )
            , test "a match against oneself is ignored" <|
                \_ ->
                    play points 2 [ ( 0, 0, ( 1, 0 ) ) ]
                        |> Expect.equal (play points 2 [])
            , test "a match against an unknown player is ignored" <|
                \_ ->
                    play points 2 [ ( 0, 5, ( 1, 0 ) ) ]
                        |> Expect.equal (play points 2 [])
            ]
        , describe "adding and removing players"
            [ test "an added player has no results" <|
                \_ ->
                    play points 2 [ ( 0, 1, ( 1, 0 ) ) ]
                        |> RR.addPlayer 7
                        |> (\state -> ( RR.cell 0 7 state, RR.cell 7 1 state, RR.isFinished state ))
                        |> Expect.equal ( Open, Open, False )
            , test "adding a player twice keeps one" <|
                \_ ->
                    RR.start points [ 0, 1 ]
                        |> RR.addPlayer 1
                        |> .game
                        |> .players
                        |> List.length
                        |> Expect.equal 2
            , test "played results of a removed player stay" <|
                \_ ->
                    RR.cell 2 0 withRemoved
                        |> Expect.equal (Played ( 4, 0 ))
            , test "unplayed matches of a removed player are forfeits" <|
                \_ ->
                    ( RR.cell 1 2 withRemoved, RR.cell 2 1 withRemoved )
                        |> Expect.equal ( Forfeit True, Forfeit False )
            , test "a forfeit gives 3 match points and no scores" <|
                \_ ->
                    RR.standings withRemoved
                        |> List.filter (.id >> (==) 1)
                        |> List.map (\s -> ( s.matchPoints, s.scored, s.conceded ))
                        |> Expect.equal [ ( 3, 1, 2 ) ]
            , test "a removed player gets no place while their forfeits count" <|
                \_ ->
                    places withRemoved
                        |> Expect.equal [ ( 1, Just 1 ), ( 0, Just 2 ), ( 2, Nothing ) ]
            , test "a forfeit can't be cleared or changed" <|
                \_ ->
                    withRemoved
                        |> RR.update (ClearResult 1 2)
                        |> RR.update (SetResult 2 1 ( 3, 0 ))
                        |> Expect.equal withRemoved
            , test "a played match with a removed player can't be cleared" <|
                \_ ->
                    withRemoved
                        |> RR.update (ClearResult 0 2)
                        |> Expect.equal withRemoved
            , test "a match between two removed players is cancelled" <|
                \_ ->
                    withRemoved
                        |> RR.removePlayer 1
                        |> RR.cell 1 2
                        |> Expect.equal Cancelled
            ]
        , describe "finished"
            [ test "not finished with an open match" <|
                \_ ->
                    play points 3 [ ( 0, 1, ( 1, 0 ) ) ]
                        |> RR.isFinished
                        |> Expect.equal False
            , test "forfeits count as results" <|
                \_ ->
                    RR.isFinished withRemoved
                        |> Expect.equal True
            , test "a rematch needs both matches" <|
                \_ ->
                    play (settings Rematch Points ByResults) 2 [ ( 0, 1, ( 1, 0 ) ) ]
                        |> RR.isFinished
                        |> Expect.equal False
            , test "no placements before the end" <|
                \_ ->
                    play points 3 [ ( 0, 1, ( 1, 0 ) ) ]
                        |> RR.placements
                        |> Expect.equal []
            , test "the placements are the places 1 to 3" <|
                \_ ->
                    play points
                        4
                        [ ( 0, 1, ( 1, 0 ) )
                        , ( 0, 2, ( 1, 0 ) )
                        , ( 0, 3, ( 1, 0 ) )
                        , ( 1, 2, ( 1, 0 ) )
                        , ( 1, 3, ( 1, 0 ) )
                        , ( 2, 3, ( 1, 0 ) )
                        ]
                        |> RR.placements
                        |> Expect.equal [ Placement 1 [ 0 ], Placement 2 [ 1 ], Placement 3 [ 2 ] ]
            , test "shared places can have more than 3 players" <|
                \_ ->
                    play winnerOnly
                        4
                        [ ( 0, 1, ( 1, 0 ) )
                        , ( 0, 2, ( 1, 0 ) )
                        , ( 0, 3, ( 1, 0 ) )
                        , ( 1, 2, ( 0, 0 ) )
                        , ( 1, 3, ( 0, 0 ) )
                        , ( 2, 3, ( 0, 0 ) )
                        ]
                        |> RR.placements
                        |> Expect.equal [ Placement 1 [ 0 ], Placement 2 [ 1, 2, 3 ] ]
            ]
        , describe "encode and decode round-trip"
            [ test "a game with results and a removed player" <|
                \_ -> roundTrip (RR.addPlayer 3 withRemoved)
            , describe "the settings" <|
                List.map
                    (\s -> test (Debug.toString s) <| \_ -> roundTrip (RR.start s [ 0, 1 ]))
                    [ settings Single Points ByResults
                    , settings Rematch Points ByTotalPoints
                    , settings Single WinnerOnly ByResults
                    , settings Rematch WinnerOnly ByTotalPoints
                    ]
            ]
        ]
