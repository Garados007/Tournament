module TournamentTest exposing (suite)

import Bracket.DoubleElimination
import Bracket.Knockout exposing (HighlightMsg(..), Msg(..), Places(..))
import Bracket.Knockout.Placement
import Bracket.RoundRobin exposing (Matches(..), Ranking(..))
import Bracket.RoundRobin.View
import Bracket.Scoring exposing (Scoring(..))
import Bracket.Stepladder
import Bracket.Swiss
import Bracket.Swiss.View
import Dict
import Expect
import Helper exposing (knockoutSettings, setWinner, startWith, startWithPlacement)
import PlayerId exposing (PlayerId)
import Standings
import Test exposing (Test, describe, test)
import Tournament exposing (Entrants(..), ModeSettings(..), Msg(..), Order(..), Tournament(..))


knockoutState : Tournament -> Maybe Bracket.Knockout.State
knockoutState tournament =
    case tournament of
        Knockout s ->
            Just s

        _ ->
            Nothing


roundRobinSettings : Bracket.RoundRobin.Settings
roundRobinSettings =
    { matches = Single, scoring = Points, ranking = ByResults }


roundRobin : Tournament.Settings
roundRobin =
    { order = Seeded
    , entrants = Tournament.StillIn
    , mode = RoundRobinSettings roundRobinSettings
    }


{-| Player 0 wins against 1 and 2, player 1 wins against 2.
-}
roundRobinPlayed : Tournament
roundRobinPlayed =
    List.foldl
        (\( row, column ) ->
            Tournament.update
                (RoundRobinMsg <| Bracket.RoundRobin.View.SetOutcome row column ( 1, 0 ))
                Tournament.initLocal
                >> Tuple.second
        )
        (Tournament.start roundRobin [ 0, 1, 2 ])
        [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ]


knockout : Tournament.Settings
knockout =
    { order = Shuffled
    , entrants = Tournament.StillIn
    , mode = KnockoutSettings (knockoutSettings Playoff)
    }


{-| A knockout tournament with 4 players where P0 and P2 won their
semi-finals.
-}
state : Bracket.Knockout.State
state =
    { settings = knockoutSettings Playoff
    , game =
        startWithPlacement 4
            |> setWinner False ( 1, 0 )
            |> setWinner False ( 1, 1 )
    }


{-| The players of the list who take part in the next tournament with both
choices: only players still in and everyone except removed.
-}
entrantsOf : List Int -> Maybe Tournament -> ( List Int, List Int )
entrantsOf players tournament =
    ( List.filter (Tournament.takesPart StillIn tournament) players
    , List.filter (Tournament.takesPart AllButRemoved tournament) players
    )


{-| Plays every match of the double elimination until it is finished. The
first player of a match always wins.
-}
playOutDoubleElimination : Tournament -> Tournament
playOutDoubleElimination tournament =
    case tournament of
        DoubleElimination s ->
            case
                Bracket.Knockout.keyedMatches s.game
                    |> List.filter
                        (\( key, match ) ->
                            Bracket.DoubleElimination.playable key s.game
                                && Bracket.Knockout.canSetWinner match s.game
                        )
                    |> List.head
            of
                Just ( key, _ ) ->
                    Tournament.update (DoubleEliminationMsg <| WinGame False key) Tournament.initLocal tournament
                        |> Tuple.second
                        |> playOutDoubleElimination

                Nothing ->
                    tournament

        _ ->
            tournament


entrantsTests : Test
entrantsTests =
    describe "takesPart"
        [ test "without a tournament everyone takes part" <|
            \_ ->
                entrantsOf [ 0, 1, 2 ] Nothing
                    |> Expect.equal ( [ 0, 1, 2 ], [ 0, 1, 2 ] )
        , test "knockout: eliminated players only take part with everyone except removed" <|
            \_ ->
                Knockout
                    { settings = knockoutSettings NoThirdPlace
                    , game =
                        startWith NoThirdPlace 4
                            |> setWinner False ( 1, 0 )
                            |> setWinner False ( 1, 1 )
                    }
                    |> Tournament.removePlayer 0
                    |> Just
                    |> entrantsOf [ 0, 1, 2, 3, 4 ]
                    |> Expect.equal ( [ 2, 4 ], [ 1, 2, 3, 4 ] )
        , test "double elimination: eliminated players only take part with everyone except removed, the top 3 always" <|
            \_ ->
                let
                    finished =
                        Tournament.start { knockout | mode = Tournament.modeDefaults Tournament.DoubleEliminationMode } [ 0, 1, 2, 3 ]
                            |> playOutDoubleElimination

                    placed place =
                        Tournament.standings finished
                            |> Standings.podium
                            |> List.filter (Tuple.first >> (==) place)
                            |> List.concatMap Tuple.second

                    runnerUp =
                        placed 2

                    without ids =
                        List.filter (\id -> not <| List.member id runnerUp) ids
                in
                finished
                    |> (\t -> List.foldl Tournament.removePlayer t runnerUp)
                    |> Just
                    |> entrantsOf [ 0, 1, 2, 3 ]
                    |> Expect.equal
                        ( without <| List.sort <| placed 1 ++ placed 2 ++ placed 3
                        , without [ 0, 1, 2, 3 ]
                        )
        , test "stepladder: eliminated players only take part with everyone except removed" <|
            \_ ->
                stepladderPlayed
                    |> Tournament.removePlayer 2
                    |> Just
                    |> entrantsOf [ 0, 1, 2 ]
                    |> Expect.equal ( [ 0 ], [ 0, 1 ] )
        , test "round robin: only removed players are left out with both choices" <|
            \_ ->
                roundRobinPlayed
                    |> Tournament.removePlayer 2
                    |> Just
                    |> entrantsOf [ 0, 1, 2 ]
                    |> Expect.equal ( [ 0, 1 ], [ 0, 1 ] )
        , test "Swiss: only removed players are left out with both choices" <|
            \_ ->
                swissPlayed
                    |> Tournament.removePlayer 3
                    |> Just
                    |> entrantsOf [ 0, 1, 2, 3 ]
                    |> Expect.equal ( [ 0, 1, 2 ], [ 0, 1, 2 ] )
        ]


{-| Removing the player and restoring them right away gives the tournament
from before.
-}
expectRestored : PlayerId -> Tournament -> Expect.Expectation
expectRestored id tournament =
    Tournament.removePlayer id tournament
        |> Tournament.restorePlayer id
        |> Expect.equal (Just tournament)


knockoutSend : Bracket.Knockout.Msg -> Tournament -> Tournament
knockoutSend msg =
    Tournament.update (KnockoutMsg msg) Tournament.initLocal >> Tuple.second


stepladderSend : Bracket.Stepladder.Msg -> Tournament -> Tournament
stepladderSend msg =
    Tournament.update (StepladderMsg msg) Tournament.initLocal >> Tuple.second


swissSend : Bracket.Swiss.View.Msg -> Tournament -> Tournament
swissSend msg =
    Tournament.update (SwissMsg msg) Tournament.initLocal >> Tuple.second


{-| P0 plays P1 and P2 plays P3 in the semi-finals, the losers play for the
3rd place.
-}
knockoutStart : Tournament
knockoutStart =
    Knockout { settings = knockoutSettings Playoff, game = startWithPlacement 4 }


restoreTests : Test
restoreTests =
    describe "restorePlayer"
        [ test "a player who isn't removed can't be restored" <|
            \_ ->
                Tournament.restorePlayer 1 knockoutStart
                    |> Expect.equal Nothing
        , describe "knockout"
            [ test "restoring right away undoes the walkover" <|
                \_ ->
                    expectRestored 1 knockoutStart
            , test "restoring right away undoes the walkover in the final" <|
                \_ ->
                    expectRestored 0 (Knockout state)
            , test "a player who was already out is only restored" <|
                \_ ->
                    Knockout
                        { settings = knockoutSettings NoThirdPlace
                        , game =
                            startWith NoThirdPlace 4
                                |> setWinner False ( 1, 0 )
                        }
                        |> expectRestored 1
            , test "the walkovers are undone while the winners haven't played on" <|
                \_ ->
                    knockoutStart
                        |> Tournament.removePlayer 1
                        |> knockoutSend (WinGame False ( 1, 1 ))
                        |> Tournament.restorePlayer 1
                        |> Expect.equal (Just <| knockoutSend (WinGame False ( 1, 1 )) knockoutStart)
            , test "can't be restored once the winner of the walkover played on" <|
                \_ ->
                    knockoutStart
                        |> Tournament.removePlayer 1
                        |> knockoutSend (WinGame False ( 1, 1 ))
                        |> knockoutSend (WinGame False ( 0, 0 ))
                        |> Tournament.restorePlayer 1
                        |> Expect.equal Nothing
            ]
        , describe "double elimination"
            [ test "restoring right away undoes the walkovers in both brackets" <|
                \_ ->
                    Tournament.start { knockout | mode = Tournament.modeDefaults Tournament.DoubleEliminationMode } [ 0, 1, 2, 3 ]
                        |> expectRestored 1
            , test "can't be restored once the winners played on" <|
                \_ ->
                    Tournament.start { knockout | mode = Tournament.modeDefaults Tournament.DoubleEliminationMode } [ 0, 1, 2, 3 ]
                        |> Tournament.removePlayer 1
                        |> playOutDoubleElimination
                        |> Tournament.restorePlayer 1
                        |> Expect.equal Nothing
            ]
        , describe "round robin"
            [ test "restoring turns the forfeits back into open matches" <|
                \_ ->
                    expectRestored 2 roundRobinPlayed
            ]
        , describe "Swiss"
            [ test "restoring right away turns the forfeits back into open matches" <|
                \_ ->
                    expectRestored 3 swissPlayed
            , test "can't be restored once the next round was paired" <|
                \_ ->
                    Tournament.start swiss [ 0, 1, 2, 3 ]
                        |> swissSend (Bracket.Swiss.View.SetOutcome 0 2 ( 1, 0 ))
                        |> Tournament.removePlayer 3
                        |> swissSend Bracket.Swiss.View.NextRound
                        |> Tournament.restorePlayer 3
                        |> Expect.equal Nothing
            ]
        , describe "stepladder"
            [ test "a player who hasn't played returns to their place" <|
                \_ ->
                    expectRestored 1 (Tournament.start stepladder [ 0, 1, 2 ])
            , test "the top seed returns after the first result" <|
                \_ ->
                    Tournament.start stepladder [ 0, 1, 2 ]
                        |> stepladderSend (Bracket.Stepladder.WinGame True 0)
                        |> expectRestored 0
            , test "a player who hasn't played can't return once their step has a result" <|
                \_ ->
                    Tournament.start stepladder [ 0, 1, 2, 3 ]
                        |> Tournament.removePlayer 1
                        |> stepladderSend (Bracket.Stepladder.WinGame True 0)
                        |> stepladderSend (Bracket.Stepladder.WinGame True 1)
                        |> Tournament.restorePlayer 1
                        |> Expect.equal Nothing
            , test "a player who hasn't played can't return if a played match would become longer" <|
                \_ ->
                    Tournament.start { stepladder | mode = StepladderSettings { bestOf = 1, finalBestOf = Just 3 } } [ 0, 1, 2 ]
                        |> stepladderSend (Bracket.Stepladder.WinGame True 0)
                        |> Tournament.removePlayer 0
                        |> Tournament.restorePlayer 0
                        |> Expect.equal Nothing
            , test "restoring right away undoes the walkover" <|
                \_ ->
                    Tournament.start stepladder [ 0, 1, 2 ]
                        |> stepladderSend (Bracket.Stepladder.WinGame True 0)
                        |> expectRestored 1
            , test "can't be restored once the winner of the walkover played on" <|
                \_ ->
                    Tournament.start stepladder [ 0, 1, 2, 3 ]
                        |> stepladderSend (Bracket.Stepladder.WinGame True 0)
                        |> Tournament.removePlayer 2
                        |> stepladderSend (Bracket.Stepladder.WinGame False 2)
                        |> Tournament.restorePlayer 2
                        |> Expect.equal Nothing
            , test "a player who was already out is only restored" <|
                \_ ->
                    expectRestored 2 stepladderPlayed
            ]
        ]


suite : Test
suite =
    describe "Tournament"
        [ entrantsTests
        , restoreTests
        , test "the default settings start a shuffled knockout tournament" <|
            \_ ->
                Tournament.defaultSettings
                    |> Expect.equal knockout
        , test "a knockout tournament needs 2 players" <|
            \_ ->
                Tournament.minPlayers knockout.mode
                    |> Expect.equal 2
        , describe "start"
            [ test "starts a knockout bracket with placements" <|
                \_ ->
                    Tournament.start knockout [ 0, 1, 2, 3 ]
                        |> Expect.equal
                            (Knockout
                                { settings = knockoutSettings Playoff
                                , game = startWithPlacement 4
                                }
                            )
            , test "keeps the players in list order" <|
                \_ ->
                    Tournament.start knockout [ 3, 1, 2 ]
                        |> Expect.equal
                            (Knockout
                                { settings = knockoutSettings Playoff
                                , game =
                                    Bracket.Knockout.start [ 3, 1, 2 ]
                                        |> Bracket.Knockout.Placement.apply (knockoutSettings Playoff)
                                }
                            )
            , test "a seeded knockout places the players by their seeds" <|
                \_ ->
                    Tournament.start { knockout | order = Seeded } [ 3, 1, 2, 0, 4 ]
                        |> Expect.equal
                            (Knockout
                                { settings = knockoutSettings Playoff
                                , game =
                                    Bracket.Knockout.startSeeded [ 3, 1, 2, 0, 4 ]
                                        |> Bracket.Knockout.Placement.apply (knockoutSettings Playoff)
                                }
                            )
            , test "stores the mode settings" <|
                \_ ->
                    Tournament.start
                        { knockout | mode = KnockoutSettings (knockoutSettings Shared) }
                        [ 0, 1 ]
                        |> knockoutState
                        |> Maybe.map .settings
                        |> Expect.equal (Just (knockoutSettings Shared))
            ]
        , test "addPlayer delegates to knockout" <|
            \_ ->
                Tournament.addPlayer 4 (Knockout state)
                    |> Expect.equal
                        (Knockout
                            { state
                                | game =
                                    Bracket.Knockout.addPlayer 4 state.game
                                        |> Bracket.Knockout.Placement.apply state.settings
                            }
                        )
        , test "removePlayer delegates to knockout" <|
            \_ ->
                Tournament.removePlayer 2 (Knockout state)
                    |> Expect.equal (Knockout { state | game = Bracket.Knockout.removePlayer 2 state.game })
        , test "update delegates to knockout" <|
            \_ ->
                Tournament.update (KnockoutMsg (WinGame True ( 0, 0 ))) Tournament.initLocal (Knockout state)
                    |> Expect.equal ( Tournament.initLocal, Knockout { state | game = setWinner True ( 0, 0 ) state.game } )
        , test "a highlight only changes the local state" <|
            \_ ->
                Tournament.update (KnockoutMsg (Highlight (TapPlayer 2))) Tournament.initLocal (Knockout state)
                    |> Tuple.mapFirst .highlight
                    |> Expect.equal ( Just 2, Knockout state )
        , test "viewers may highlight players, but not change results" <|
            \_ ->
                List.map Tournament.isViewerMsg
                    [ KnockoutMsg (Highlight LeaveBracket)
                    , KnockoutMsg (WinGame True ( 0, 0 ))
                    , KnockoutMsg (Undo ( 0, 0 ))
                    ]
                    |> Expect.equal [ True, False, False ]
        , test "viewers may expand earlier Swiss rounds, but not change results" <|
            \_ ->
                List.map Tournament.isViewerMsg
                    [ SwissMsg (Bracket.Swiss.View.ToggleRound 1)
                    , SwissMsg Bracket.Swiss.View.NextRound
                    , RoundRobinMsg (Bracket.RoundRobin.View.Clear 0 1)
                    ]
                    |> Expect.equal [ True, False, False ]
        , test "isEliminated delegates to knockout" <|
            \_ ->
                List.map (\id -> Tournament.isEliminated id (Knockout state)) [ 0, 1, 2, 3, 4 ]
                    |> Expect.equal (List.map (\id -> Bracket.Knockout.isEliminated id state.game) [ 0, 1, 2, 3, 4 ])
        , test "standings delegate to knockout" <|
            \_ ->
                Knockout { state | game = setWinner True ( 0, 0 ) state.game }
                    |> Tournament.standings
                    |> Standings.podium
                    |> Expect.equal [ ( 1, [ 2 ] ), ( 2, [ 0 ] ) ]
        , test "player names aren't stored in the tournament" <|
            \_ ->
                Tournament.start knockout [ 0, 1 ]
                    |> knockoutState
                    |> Maybe.map (.game >> .players >> Dict.values)
                    |> Expect.equal (Just [ { id = 0, alive = True, removed = False }, { id = 1, alive = True, removed = False } ])
        , describe "round robin"
            [ test "starts with the players in list order" <|
                \_ ->
                    Tournament.start roundRobin [ 2, 0, 1 ]
                        |> Expect.equal (RoundRobin <| Bracket.RoundRobin.start roundRobinSettings [ 2, 0, 1 ])
            , test "needs 2 players" <|
                \_ ->
                    Tournament.minPlayers roundRobin.mode
                        |> Expect.equal 2
            , test "update stores the result" <|
                \_ ->
                    case roundRobinPlayed of
                        RoundRobin rr ->
                            Bracket.RoundRobin.cell 1 0 rr
                                |> Expect.equal (Bracket.Scoring.Played ( 0, 1 ))

                        _ ->
                            Expect.fail "not a round robin"
            , test "a message of another mode is ignored" <|
                \_ ->
                    Tournament.update (KnockoutMsg (WinGame True ( 0, 0 ))) Tournament.initLocal roundRobinPlayed
                        |> Expect.equal ( Tournament.initLocal, roundRobinPlayed )
            , test "the podium once every match is played" <|
                \_ ->
                    Tournament.standings roundRobinPlayed
                        |> Standings.podium
                        |> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 1 ] ), ( 3, [ 2 ] ) ]
            , test "only removed players are eliminated" <|
                \_ ->
                    Tournament.removePlayer 1 roundRobinPlayed
                        |> (\t -> List.map (\id -> Tournament.isEliminated id t) [ 0, 1, 2 ])
                        |> Expect.equal [ False, True, False ]
            , test "an added player opens new matches" <|
                \_ ->
                    Tournament.addPlayer 3 roundRobinPlayed
                        |> Tournament.standings
                        |> Standings.podium
                        |> Expect.equal []
            , test "uses the header control instead of hiding matches" <|
                \_ ->
                    Tournament.controls roundRobinPlayed
                        |> Expect.equal { hideFinished = False, headers = True }
            ]
        , describe "swiss"
            [ test "starts with the players in list order" <|
                \_ ->
                    Tournament.start swiss [ 2, 0, 1, 3 ]
                        |> Expect.equal (Swiss <| Bracket.Swiss.start Bracket.Swiss.defaultSettings [ 2, 0, 1, 3 ])
            , test "needs 2 players" <|
                \_ ->
                    Tournament.minPlayers swiss.mode
                        |> Expect.equal 2
            , test "update plays the rounds until the tournament is finished, with shared places" <|
                \_ ->
                    swissPlayed
                        |> Tournament.standings
                        |> Standings.podium
                        |> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 1, 2 ] ) ]
            , test "only removed players are eliminated" <|
                \_ ->
                    Tournament.removePlayer 1 swissPlayed
                        |> (\t -> List.map (\id -> Tournament.isEliminated id t) [ 0, 1, 2 ])
                        |> Expect.equal [ False, True, False ]
            , test "offers a CSV of the standings" <|
                \_ ->
                    Tournament.csv swissPlayed
                        |> Maybe.map (\toCsv -> String.lines (toCsv Dict.empty) |> List.length)
                        |> Expect.equal (Just 6)
            ]
        , describe "stepladder"
            [ test "starts with the first player at the top" <|
                \_ ->
                    Tournament.start stepladder [ 2, 0, 1 ]
                        |> Expect.equal
                            (Stepladder
                                { settings = Bracket.Stepladder.defaultSettings
                                , game = Bracket.Stepladder.start [ 2, 0, 1 ]
                                }
                            )
            , test "needs 2 players" <|
                \_ ->
                    Tournament.minPlayers stepladder.mode
                        |> Expect.equal 2
            , test "update plays the matches up the ladder" <|
                \_ ->
                    stepladderPlayed
                        |> Tournament.standings
                        |> Standings.podium
                        |> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 1 ] ), ( 3, [ 2 ] ) ]
            , test "the losers are eliminated" <|
                \_ ->
                    List.map (\id -> Tournament.isEliminated id stepladderPlayed) [ 0, 1, 2 ]
                        |> Expect.equal [ False, True, True ]
            , test "viewers can't change results" <|
                \_ ->
                    Tournament.isViewerMsg (StepladderMsg (Bracket.Stepladder.Undo 1))
                        |> Expect.equal False
            , test "offers no CSV" <|
                \_ ->
                    Tournament.csv stepladderPlayed
                        |> Expect.equal Nothing
            ]
        ]


stepladder : Tournament.Settings
stepladder =
    { order = Seeded
    , entrants = Tournament.StillIn
    , mode = Tournament.modeDefaults Tournament.StepladderMode
    }


{-| Player 0 is the top seed. 1 beats 2 at the bottom, then loses to 0 in the
final.
-}
stepladderPlayed : Tournament
stepladderPlayed =
    let
        send msg =
            Tournament.update (StepladderMsg msg) Tournament.initLocal >> Tuple.second
    in
    Tournament.start stepladder [ 0, 1, 2 ]
        |> send (Bracket.Stepladder.WinGame True 0)
        |> send (Bracket.Stepladder.WinGame True 1)


swiss : Tournament.Settings
swiss =
    { order = Seeded
    , entrants = Tournament.StillIn
    , mode = Tournament.modeDefaults Tournament.SwissMode
    }


{-| 4 players in 2 rounds. The higher seed always wins, so 1 and 2 end with
the same points and Buchholz.
-}
swissPlayed : Tournament
swissPlayed =
    let
        send msg =
            Tournament.update (SwissMsg msg) Tournament.initLocal >> Tuple.second
    in
    Tournament.start swiss [ 0, 1, 2, 3 ]
        |> send (Bracket.Swiss.View.SetOutcome 0 2 ( 1, 0 ))
        |> send (Bracket.Swiss.View.SetOutcome 1 3 ( 1, 0 ))
        |> send Bracket.Swiss.View.NextRound
        |> send (Bracket.Swiss.View.SetOutcome 0 1 ( 1, 0 ))
        |> send (Bracket.Swiss.View.SetOutcome 2 3 ( 1, 0 ))
