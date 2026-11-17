module StandingsTest exposing (suite)

import Bracket.DoubleElimination as DE
import Bracket.Knockout as Knockout exposing (Msg(..), Places(..))
import Bracket.Knockout.Placement
import Bracket.RoundRobin as RR
import Bracket.Scoring exposing (Scoring(..))
import Bracket.Stepladder as Stepladder
import Bracket.Swiss as Swiss
import Dict
import Expect exposing (Expectation)
import Helper exposing (knockoutSettings, players, setWinner, setWinnerWith, startWith)
import List.Extra
import Standings exposing (Row, Standings, Status(..))
import Test exposing (Test, describe, test)


summary : Standings -> List ( Int, Maybe Int, Status )
summary =
    .rows >> List.map (\r -> ( r.id, r.place, r.status ))


{-| Won, drawn and lost with the mode-specific values.
-}
stats : Standings -> List ( Int, ( Int, Int, Int ), List String )
stats =
    .rows >> List.map (\r -> ( r.id, ( r.won, r.drawn, r.lost ), r.extra ))


placed : Int -> Int -> ( Int, Maybe Int, Status )
placed id place =
    ( id, Just place, Placed )


stillIn : Int -> ( Int, Maybe Int, Status )
stillIn id =
    ( id, Nothing, StillIn )


out : Int -> ( Int, Maybe Int, Status )
out id =
    ( id, Nothing, Out )


{-| Every place is one more than the number of players with a better place.
-}
expectCompetitionRanking : Standings -> Expectation
expectCompetitionRanking standings =
    let
        places =
            List.filterMap .place standings.rows
    in
    places
        |> List.filter (\place -> place /= 1 + List.length (List.filter (\other -> other < place) places))
        |> Expect.equal []



-- KNOCKOUT


knockout : Places -> Knockout.Game -> Standings
knockout places =
    Bracket.Knockout.Placement.toStandings (knockoutSettings places)


{-| 8 players: player 1 of every match wins, P4 wins the final.
-}
quarterFinals : Places -> Knockout.Game
quarterFinals places =
    startWith places 8
        |> setWinner False ( 2, 0 )
        |> setWinner False ( 2, 1 )
        |> setWinner False ( 2, 2 )
        |> setWinner False ( 2, 3 )


semiFinals : Places -> Knockout.Game
semiFinals places =
    quarterFinals places
        |> setWinner False ( 1, 0 )
        |> setWinner False ( 1, 1 )


final : Places -> Knockout.Game
final places =
    semiFinals places
        |> (if places == Playoff then
                setWinner False ( 0, 1 )

            else
                identity
           )
        |> setWinner True ( 0, 0 )


type alias Case =
    { name : String
    , standings : Standings
    , expected : List ( Int, Maybe Int, Status )
    }


knockoutCases : List Case
knockoutCases =
    [ { name = "the quarter-final losers share the 5th place, the others are still in"
      , standings = knockout Playoff (quarterFinals Playoff)
      , expected = [ placed 1 5, placed 3 5, placed 5 5, placed 7 5, stillIn 0, stillIn 2, stillIn 4, stillIn 6 ]
      }
    , { name = "the players of the 3rd place match come after the finalists"
      , standings = knockout Playoff (semiFinals Playoff)
      , expected = [ placed 1 5, placed 3 5, placed 5 5, placed 7 5, stillIn 0, stillIn 4, stillIn 2, stillIn 6 ]
      }
    , { name = "the 3rd place match decides the places 3 and 4"
      , standings = knockout Playoff (final Playoff)
      , expected = [ placed 4 1, placed 0 2, placed 2 3, placed 6 4, placed 1 5, placed 3 5, placed 5 5, placed 7 5 ]
      }
    , { name = "shared: the semi-final losers share the 3rd place"
      , standings = knockout Shared (semiFinals Shared)
      , expected = [ placed 2 3, placed 6 3, placed 1 5, placed 3 5, placed 5 5, placed 7 5, stillIn 0, stillIn 4 ]
      }
    , { name = "shared: the place after the shared 3rd place is 5th"
      , standings = knockout Shared (final Shared)
      , expected = [ placed 4 1, placed 0 2, placed 2 3, placed 6 3, placed 1 5, placed 3 5, placed 5 5, placed 7 5 ]
      }
    , { name = "only 1st and 2nd: the others have no place, sorted by the round they reached"
      , standings = knockout NoThirdPlace (final NoThirdPlace)
      , expected = [ placed 4 1, placed 0 2, out 2, out 6, out 1, out 3, out 5, out 7 ]
      }
    , { name = "only 1st and 2nd: players still in come before players who are out"
      , standings = knockout NoThirdPlace (semiFinals NoThirdPlace)
      , expected = [ stillIn 0, stillIn 4, out 2, out 6, out 1, out 3, out 5, out 7 ]
      }
    , { name = "the pre-round losers share the place after the main round"
      , standings =
            startWith Playoff 6
                |> setWinner False ( 2, 0 )
                |> setWinner False ( 2, 1 )
                |> knockout Playoff
      , expected = [ placed 3 5, placed 5 5, stillIn 0, stillIn 1, stillIn 2, stillIn 4 ]
      }
    , { name = "3 players: the pre-round loser is 3rd"
      , standings =
            startWith Playoff 3
                |> setWinner False ( 1, 0 )
                |> knockout Playoff
      , expected = [ placed 2 3, stillIn 0, stillIn 1 ]
      }
    , { name = "play out all: the places come from the placement matches"
      , standings =
            startWith PlayAll 4
                |> setWinner False ( 1, 0 )
                |> setWinner False ( 1, 1 )
                |> knockout PlayAll
      , expected = [ stillIn 0, stillIn 2, stillIn 1, stillIn 3 ]
      }
    , { name = "play out all: every player gets the place of their placement match"
      , standings =
            startWith PlayAll 4
                |> setWinner False ( 1, 0 )
                |> setWinner False ( 1, 1 )
                |> setWinner True ( -300, 0 )
                |> setWinner True ( 0, 0 )
                |> knockout PlayAll
      , expected = [ placed 2 1, placed 0 2, placed 3 3, placed 1 4 ]
      }
    , { name = "a removed player loses by walkover and comes last without a place"
      , standings =
            startWith Playoff 8
                |> Knockout.removePlayer 1
                |> knockout Playoff
      , expected = [ stillIn 0, stillIn 2, stillIn 3, stillIn 4, stillIn 5, stillIn 6, stillIn 7, ( 1, Nothing, Removed ) ]
      }
    , { name = "a player removed after their loss keeps the place"
      , standings =
            startWith Playoff 8
                |> Knockout.removePlayer 1
                |> setWinner False ( 2, 1 )
                |> Knockout.removePlayer 3
                |> knockout Playoff
      , expected = [ ( 3, Just 5, Removed ), stillIn 0, stillIn 2, stillIn 4, stillIn 5, stillIn 6, stillIn 7, ( 1, Nothing, Removed ) ]
      }
    ]


bestOf3 : Knockout.Settings
bestOf3 =
    { places = Playoff, bestOf = 3, finalBestOf = Nothing }


knockoutTests : Test
knockoutTests =
    describe "knockout"
        [ describe "places" <|
            List.map (\c -> test c.name <| \_ -> summary c.standings |> Expect.equal c.expected) knockoutCases
        , test "the stats count the matches, the column shows where a player lost" <|
            \_ ->
                knockout Playoff (final Playoff)
                    |> stats
                    |> Expect.equal
                        [ ( 4, ( 3, 0, 0 ), [ "" ] )
                        , ( 0, ( 2, 0, 1 ), [ "Final" ] )
                        , ( 2, ( 2, 0, 1 ), [ "Semi-final" ] )
                        , ( 6, ( 1, 0, 2 ), [ "Semi-final" ] )
                        , ( 1, ( 0, 0, 1 ), [ "Quarter-final" ] )
                        , ( 3, ( 0, 0, 1 ), [ "Quarter-final" ] )
                        , ( 5, ( 0, 0, 1 ), [ "Quarter-final" ] )
                        , ( 7, ( 0, 0, 1 ), [ "Quarter-final" ] )
                        ]
        , test "the rounds before the quarter-finals and the pre-round have names" <|
            \_ ->
                let
                    game =
                        startWith Playoff 20
                in
                List.map (Knockout.roundName game) [ 4, 3, 2 ]
                    |> Expect.equal [ "Pre-round", "Round of 16", "Quarter-final" ]
        , test "a walkover counts as a win and a loss" <|
            \_ ->
                startWith Playoff 8
                    |> Knockout.removePlayer 1
                    |> knockout Playoff
                    |> stats
                    |> List.filter (\( id, _, _ ) -> id == 0 || id == 1)
                    |> Expect.equal [ ( 0, ( 1, 0, 0 ), [ "" ] ), ( 1, ( 0, 0, 1 ), [ "Quarter-final" ] ) ]
        , test "longer matches show the games won and lost" <|
            \_ ->
                startWith Playoff 4
                    |> setWinnerWith bestOf3 False ( 1, 0 )
                    |> setWinnerWith bestOf3 True ( 1, 0 )
                    |> setWinnerWith bestOf3 False ( 1, 0 )
                    |> Bracket.Knockout.Placement.toStandings bestOf3
                    |> Expect.all
                        [ .columns >> Expect.equal [ "Out in", "Games" ]
                        , stats
                            >> List.filter (\( id, _, _ ) -> id <= 1)
                            >> Expect.equal [ ( 0, ( 1, 0, 0 ), [ "", "2 : 1" ] ), ( 1, ( 0, 0, 1 ), [ "Semi-final", "1 : 2" ] ) ]
                        ]
        , test "single games have no games column" <|
            \_ ->
                knockout Playoff (final Playoff)
                    |> Expect.all [ .columns >> Expect.equal [ "Out in" ], .draws >> Expect.equal False ]
        , describe "finished"
            [ test "not before the last match" <|
                \_ -> (knockout Playoff (semiFinals Playoff)).finished |> Expect.equal False
            , test "after the final and the 3rd place match" <|
                \_ -> (knockout Playoff (final Playoff)).finished |> Expect.equal True
            , test "with only 1st and 2nd after the final" <|
                \_ -> (knockout NoThirdPlace (final NoThirdPlace)).finished |> Expect.equal True
            ]
        , describe "places played out by player 1 follow the competition ranking" <|
            List.concatMap
                (\( name, places ) ->
                    List.map
                        (\n ->
                            test (name ++ ", " ++ String.fromInt n ++ " players") <|
                                \_ ->
                                    knockoutPlayOut (startWith places n)
                                        |> knockout places
                                        |> Expect.all
                                            [ expectCompetitionRanking
                                            , .finished >> Expect.equal True
                                            , .rows >> List.map .status >> List.all ((==) Placed) >> Expect.equal True
                                            , .rows >> List.length >> Expect.equal n
                                            ]
                        )
                        (List.range 2 17)
                )
                [ ( "Playoff", Playoff ), ( "Shared", Shared ), ( "PlayAll", PlayAll ) ]
        ]


knockoutPlayOut : Knockout.Game -> Knockout.Game
knockoutPlayOut game =
    case List.Extra.find (\( _, match ) -> Knockout.canSetWinner match game) (Knockout.keyedMatches game) of
        Just ( key, _ ) ->
            knockoutPlayOut (setWinner False key game)

        Nothing ->
            game



-- DOUBLE ELIMINATION


deWin : Bool -> Knockout.MatchKey -> Knockout.Game -> Knockout.Game
deWin isPlayer2 key =
    DE.update DE.defaultSettings (WinGame isPlayer2 key)


de : Knockout.Game -> Standings
de =
    DE.toStandings DE.defaultSettings


{-| 4 players: the winner bracket goes to player 0, the loser bracket to 2.
-}
beforeGrandFinal : Knockout.Game
beforeGrandFinal =
    DE.start DE.defaultSettings (players 4)
        |> deWin False ( 2, 0 )
        |> deWin False ( 2, 1 )
        |> deWin False ( 1, 0 )
        |> deWin False ( -3, 0 )
        |> deWin True ( -2, 0 )


dePlayOut : Knockout.Game -> Knockout.Game
dePlayOut game =
    case
        List.Extra.find
            (\( key, match ) -> DE.playable key game && Knockout.canSetWinner match game)
            (Knockout.keyedMatches game)
    of
        Just ( key, _ ) ->
            dePlayOut (deWin False key game)

        Nothing ->
            game


doubleEliminationTests : Test
doubleEliminationTests =
    describe "double elimination"
        [ test "the loser bracket decides the places from 3rd on" <|
            \_ ->
                de beforeGrandFinal
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 1 3, placed 3 4, stillIn 0, stillIn 2 ]
                        , stats
                            >> Expect.equal
                                [ ( 1, ( 1, 0, 2 ), [ "Loser bracket round 2" ] )
                                , ( 3, ( 0, 0, 2 ), [ "Loser bracket round 1" ] )
                                , ( 0, ( 2, 0, 0 ), [ "" ] )
                                , ( 2, ( 2, 0, 1 ), [ "" ] )
                                ]
                        , .finished >> Expect.equal False
                        ]
        , test "the grand final decides the first two places" <|
            \_ ->
                deWin False DE.grandFinal beforeGrandFinal
                    |> de
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 0 1, placed 2 2, placed 1 3, placed 3 4 ]
                        , stats >> List.take 2 >> List.map (\( _, _, extra ) -> extra) >> Expect.equal [ [ "" ], [ "Grand final" ] ]
                        , .finished >> Expect.equal True
                        ]
        , test "the reset match decides the first two places" <|
            \_ ->
                beforeGrandFinal
                    |> deWin True DE.grandFinal
                    |> Expect.all
                        [ de >> summary >> Expect.equal [ placed 1 3, placed 3 4, stillIn 2, stillIn 0 ]
                        , deWin True DE.resetMatch
                            >> de
                            >> stats
                            >> List.take 2
                            >> Expect.equal [ ( 0, ( 3, 0, 1 ), [ "" ] ), ( 2, ( 3, 0, 2 ), [ "Reset match" ] ) ]
                        ]
        , test "the losers of a loser bracket round share a place" <|
            \_ ->
                DE.start DE.defaultSettings (players 8)
                    |> dePlayOut
                    |> de
                    |> .rows
                    |> List.map .place
                    |> Expect.equal (List.map Just [ 1, 2, 3, 4, 5, 5, 7, 7 ])
        , describe "places played out follow the competition ranking" <|
            List.map
                (\n ->
                    test (String.fromInt n ++ " players") <|
                        \_ ->
                            DE.start DE.defaultSettings (players n)
                                |> dePlayOut
                                |> de
                                |> Expect.all
                                    [ expectCompetitionRanking
                                    , .finished >> Expect.equal True
                                    , .rows >> List.length >> Expect.equal n
                                    ]
                )
                (List.range 3 17)
        , test "a removed player loses by walkover and gets no place" <|
            \_ ->
                DE.start DE.defaultSettings (players 4)
                    |> DE.removePlayer 3
                    |> de
                    |> summary
                    |> List.Extra.last
                    |> Expect.equal (Just ( 3, Nothing, Removed ))
        ]



-- ROUND ROBIN AND SWISS


roundRobin : Scoring -> RR.Ranking -> Int -> List ( Int, Int, ( Int, Int ) ) -> RR.State
roundRobin scoring ranking n results =
    List.foldl
        (\( home, away, score ) -> RR.update (RR.SetResult home away score))
        (RR.start { matches = RR.Single, scoring = scoring, ranking = ranking } (players n))
        results


roundRobinTests : Test
roundRobinTests =
    describe "round robin"
        [ test "the table with score and match points" <|
            \_ ->
                roundRobin Points RR.ByResults 3 [ ( 0, 1, ( 2, 1 ) ), ( 0, 2, ( 1, 1 ) ), ( 1, 2, ( 0, 3 ) ) ]
                    |> RR.toStandings
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 2 1, placed 0 2, placed 1 3 ]
                        , stats
                            >> Expect.equal
                                [ ( 2, ( 1, 1, 0 ), [ "4 : 1", "4" ] )
                                , ( 0, ( 1, 1, 0 ), [ "3 : 2", "4" ] )
                                , ( 1, ( 0, 0, 2 ), [ "1 : 5", "0" ] )
                                ]
                        , .columns >> Expect.equal [ "Score", "Pts" ]
                        , .draws >> Expect.equal True
                        , .finished >> Expect.equal True
                        ]
        , test "the live ranking before the end" <|
            \_ ->
                roundRobin Points RR.ByResults 3 [ ( 0, 1, ( 1, 0 ) ) ]
                    |> RR.toStandings
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 0 1, placed 2 2, placed 1 3 ]
                        , .finished >> Expect.equal False
                        ]
        , test "a removed player loses the open matches by forfeit and comes last" <|
            \_ ->
                roundRobin Points RR.ByResults 3 [ ( 0, 1, ( 1, 0 ) ) ]
                    |> RR.removePlayer 2
                    |> RR.toStandings
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 0 1, placed 1 2, ( 2, Nothing, Removed ) ]
                        , stats >> List.map (\( _, wdl, _ ) -> wdl) >> Expect.equal [ ( 2, 0, 0 ), ( 1, 0, 1 ), ( 0, 0, 2 ) ]
                        , .finished >> Expect.equal True
                        ]
        , test "shared places leave gaps" <|
            \_ ->
                roundRobin WinnerOnly RR.ByResults 3 [ ( 0, 1, ( 1, 1 ) ), ( 0, 2, ( 1, 1 ) ), ( 1, 2, ( 1, 1 ) ) ]
                    |> RR.toStandings
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 0 1, placed 1 1, placed 2 1 ]
                        , .columns >> Expect.equal [ "Pts" ]
                        ]
        , test "the total points ranking has no match points" <|
            \_ ->
                roundRobin Points RR.ByTotalPoints 2 []
                    |> RR.toStandings
                    |> .columns
                    |> Expect.equal [ "Score" ]
        ]


swissTests : Test
swissTests =
    describe "Swiss"
        [ test "the live table with Buchholz, shared places leave gaps" <|
            \_ ->
                Swiss.start Swiss.defaultSettings (players 4)
                    |> Swiss.update (Swiss.SetResult 0 2 ( 1, 0 ))
                    |> Swiss.update (Swiss.SetResult 1 3 ( 1, 1 ))
                    |> Swiss.toStandings
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 0 1, placed 1 2, placed 3 2, placed 2 4 ]
                        , stats
                            >> Expect.equal
                                [ ( 0, ( 1, 0, 0 ), [ "1 : 0", "3", "0" ] )
                                , ( 1, ( 0, 1, 0 ), [ "1 : 1", "1", "1" ] )
                                , ( 3, ( 0, 1, 0 ), [ "1 : 1", "1", "1" ] )
                                , ( 2, ( 0, 0, 1 ), [ "0 : 1", "0", "3" ] )
                                ]
                        , .columns >> Expect.equal [ "Score", "Pts", "Buchholz" ]
                        , .finished >> Expect.equal False
                        ]
        , test "a bye counts as a win" <|
            \_ ->
                Swiss.start Swiss.defaultSettings (players 3)
                    |> Swiss.toStandings
                    |> stats
                    |> List.filter (\( id, _, _ ) -> id == 2)
                    |> List.map (\( _, wdl, _ ) -> wdl)
                    |> Expect.equal [ ( 1, 0, 0 ) ]
        , test "a removed player comes last without a place" <|
            \_ ->
                Swiss.start Swiss.defaultSettings (players 4)
                    |> Swiss.removePlayer 2
                    |> Swiss.toStandings
                    |> Expect.all
                        [ summary >> List.Extra.last >> Expect.equal (Just ( 2, Nothing, Removed ))
                        , stats >> List.Extra.last >> Expect.equal (Just ( 2, ( 0, 0, 1 ), [ "0 : 0", "0", "3" ] ))
                        ]
        ]



-- STEPLADDER


ladderWin : Bool -> Int -> Stepladder.Game -> Stepladder.Game
ladderWin isPlayer2 index =
    Stepladder.update Stepladder.defaultSettings (Stepladder.WinGame isPlayer2 index)


ladder : Stepladder.Game -> Standings
ladder =
    Stepladder.toStandings Stepladder.defaultSettings


{-| Seeds 0 (top) to 4 (bottom).
-}
five : Stepladder.Game
five =
    Stepladder.start (players 5)


stepladderTests : Test
stepladderTests =
    describe "stepladder"
        [ test "the loser of a step gets the place below the players further up" <|
            \_ ->
                ladderWin True 0 five
                    |> ladder
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 4 5, stillIn 0, stillIn 1, stillIn 3, stillIn 2 ]
                        , stats
                            >> Expect.equal
                                [ ( 4, ( 0, 0, 1 ), [ "Step 1" ] )
                                , ( 0, ( 0, 0, 0 ), [ "Final" ] )
                                , ( 1, ( 0, 0, 0 ), [ "Step 3" ] )
                                , ( 3, ( 1, 0, 0 ), [ "Step 2" ] )
                                , ( 2, ( 0, 0, 0 ), [ "Step 2" ] )
                                ]
                        , .columns >> Expect.equal [ "Step" ]
                        , .finished >> Expect.equal False
                        ]
        , test "one place per step" <|
            \_ ->
                five
                    |> ladderWin True 0
                    |> ladderWin True 1
                    |> ladderWin True 2
                    |> ladderWin False 3
                    |> ladder
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 1 1, placed 0 2, placed 2 3, placed 3 4, placed 4 5 ]
                        , .finished >> Expect.equal True
                        ]
        , test "a player removed in their match loses by walkover and gets no place" <|
            \_ ->
                ladderWin True 0 five
                    |> Stepladder.removePlayer Stepladder.defaultSettings 3
                    |> ladder
                    |> Expect.all
                        [ summary >> Expect.equal [ placed 4 5, stillIn 0, stillIn 2, stillIn 1, ( 3, Nothing, Removed ) ]
                        , stats >> List.Extra.last >> Expect.equal (Just ( 3, ( 1, 0, 1 ), [ "Step 2" ] ))
                        ]
        , test "a player removed before playing is listed without a place" <|
            \_ ->
                Stepladder.removePlayer Stepladder.defaultSettings 0 five
                    |> ladder
                    |> summary
                    |> List.Extra.last
                    |> Expect.equal (Just ( 0, Nothing, Removed ))
        , test "a player removed after their loss keeps the place" <|
            \_ ->
                ladderWin True 0 five
                    |> Stepladder.removePlayer Stepladder.defaultSettings 4
                    |> ladder
                    |> summary
                    |> List.head
                    |> Expect.equal (Just ( 4, Just 5, Removed ))
        ]



-- SHARED


row : Int -> Maybe Int -> Status -> List String -> Row
row id place status extra =
    { id = id, place = place, status = status, won = 2, drawn = 1, lost = 0, extra = extra }


sharedTests : Test
sharedTests =
    describe "shared"
        [ test "the podium groups the places 1 to 3" <|
            \_ ->
                { rows =
                    [ row 0 (Just 1) Placed []
                    , row 1 (Just 1) Placed []
                    , row 2 (Just 3) Placed []
                    , row 3 (Just 4) Placed []
                    , row 4 Nothing StillIn []
                    ]
                , finished = False
                , draws = False
                , columns = []
                }
                    |> Standings.podium
                    |> Expect.equal [ ( 1, [ 0, 1 ] ), ( 3, [ 2 ] ) ]
        , test "the CSV has the columns of the table and marks removed players" <|
            \_ ->
                { rows =
                    [ row 0 (Just 1) Placed [ "5 : 2" ]
                    , row 1 Nothing Removed [ "0 : 1" ]
                    ]
                , finished = True
                , draws = True
                , columns = [ "Score" ]
                }
                    |> Standings.csv (Dict.fromList [ ( 0, "Ann; B" ), ( 1, "Bob" ) ])
                    |> Expect.equal
                        ("\u{FEFF}Place;Name;Played;W;D;L;Score;Note\u{000D}\n"
                            ++ "1;\"Ann; B\";3;2;1;0;5 : 2;\u{000D}\n"
                            ++ ";Bob;3;2;1;0;0 : 1;removed\u{000D}\n"
                        )
        , test "the CSV of a mode without draws has no D column" <|
            \_ ->
                knockout NoThirdPlace (final NoThirdPlace)
                    |> Standings.csv (Dict.fromList (List.map (\id -> ( id, "P" ++ String.fromInt id )) (players 8)))
                    |> String.split "\u{000D}\n"
                    |> List.take 4
                    |> Expect.equal
                        [ "\u{FEFF}Place;Name;Played;W;L;Out in;Note"
                        , "1;P4;3;3;0;;"
                        , "2;P0;3;2;1;Final;"
                        , ";P2;2;1;1;Semi-final;"
                        ]
        ]


suite : Test
suite =
    describe "Standings"
        [ knockoutTests
        , doubleEliminationTests
        , roundRobinTests
        , swissTests
        , stepladderTests
        , sharedTests
        ]
