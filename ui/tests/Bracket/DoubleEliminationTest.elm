module Bracket.DoubleEliminationTest exposing (suite)

import Bracket.DoubleElimination as DE exposing (Settings)
import Bracket.DoubleElimination.Serializer as Serializer
import Bracket.Knockout as Knockout exposing (Game, MatchKey, Msg(..), PlayerSlot)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Json.Decode as JD
import List.Extra
import Set
import Standings
import Test exposing (Test, describe, test)


withReset : Settings
withReset =
    DE.defaultSettings


podium : Game -> List ( Int, List Int )
podium =
    DE.toStandings withReset >> Standings.podium


noReset : Settings
noReset =
    { withReset | reset = False }


players : Int -> List Int
players n =
    List.range 0 (n - 1)


start : Settings -> Int -> Game
start settings n =
    DE.start settings (players n)


win : Bool -> MatchKey -> Game -> Game
win isPlayer2 key =
    DE.update withReset (WinGame isPlayer2 key)


undo : MatchKey -> Game -> Game
undo key =
    DE.update withReset (Undo key)


keyed : Game -> List ( MatchKey, Knockout.Match )
keyed =
    Knockout.keyedMatches


matchesIn : (Int -> Bool) -> Game -> List ( MatchKey, Knockout.Match )
matchesIn isRound game =
    keyed game |> List.filter (\( ( round, _ ), _ ) -> isRound round)


{-| Decides who wins a match from its key and both players: `True` for player 2.
-}
type alias Strategy =
    MatchKey -> Int -> Int -> Bool


strategies : List ( String, Strategy )
strategies =
    [ ( "player 1 wins", \_ _ _ -> False )
    , ( "player 2 wins", \_ _ _ -> True )
    , ( "lower id wins", \_ a b -> b < a )
    , ( "higher id wins", \_ a b -> b > a )
    , ( "mixed", \( round, index ) a b -> modBy 3 (round + index + a + b) == 0 )
    ]


{-| The next match that can take a game.
-}
nextMatch : Game -> Maybe ( MatchKey, ( Int, Int ) )
nextMatch game =
    keyed game
        |> List.filter (\( key, match ) -> DE.playable key game && Knockout.canSetWinner match game)
        |> List.sortBy (\( ( round, index ), _ ) -> ( -(abs round), index ))
        |> List.head
        |> Maybe.andThen
            (\( key, match ) ->
                Knockout.opponents match game |> Maybe.map (Tuple.pair key)
            )


{-| Plays every match with the strategy and returns the players of every match
in the order they were played, with the winner first.
-}
playOut : Settings -> Strategy -> Game -> ( Game, List ( MatchKey, ( Int, Int ) ) )
playOut settings strategy =
    let
        loop : Int -> List ( MatchKey, ( Int, Int ) ) -> Game -> ( Game, List ( MatchKey, ( Int, Int ) ) )
        loop budget played game =
            case ( budget, nextMatch game ) of
                ( 0, _ ) ->
                    ( game, List.reverse played )

                ( _, Nothing ) ->
                    ( game, List.reverse played )

                ( _, Just ( key, ( a, b ) ) ) ->
                    let
                        isPlayer2 =
                            strategy key a b

                        result =
                            if isPlayer2 then
                                ( b, a )

                            else
                                ( a, b )
                    in
                    loop (budget - 1) (( key, result ) :: played) (DE.update settings (WinGame isPlayer2 key) game)
    in
    loop 1000 []


losses : List ( MatchKey, ( Int, Int ) ) -> Dict Int Int
losses =
    List.foldl (\( _, ( _, loser ) ) -> Dict.update loser (Maybe.withDefault 0 >> (+) 1 >> Just)) Dict.empty


{-| Every input slot is used by one match, every target outside the placement
round is the input of a match, and no two matches send a player to the same
slot.
-}
expectValidStructure : Game -> Expectation
expectValidStructure game =
    let
        inputs =
            keyed game |> List.concatMap (\( _, m ) -> [ m.player1, m.player2 ])

        targets =
            keyed game |> List.concatMap (\( _, m ) -> List.filterMap identity [ m.winner, m.loser ])

        duplicates list =
            List.filter (\x -> List.length (List.filter ((==) x) list) > 1) list
    in
    Expect.all
        [ \_ -> duplicates inputs |> Expect.equal [] |> Expect.onFail "input slots used twice"
        , \_ ->
            targets
                |> List.filter (\( round, _ ) -> round /= Knockout.placementRound)
                |> List.filter (\t -> not <| List.member t inputs)
                |> Expect.equal []
                |> Expect.onFail "targets without a match"
        , \_ -> duplicates targets |> Expect.equal [] |> Expect.onFail "two matches send a player to the same slot"
        , \_ ->
            keyed game
                |> List.filter (\( _, m ) -> m.winner == Nothing)
                |> Expect.equal []
                |> Expect.onFail "matches without a winner target"
        ]
        ()


loserTarget : MatchKey -> Game -> Maybe PlayerSlot
loserTarget key game =
    Knockout.matchAt key game |> Maybe.andThen .loser


suite : Test
suite =
    describe "Bracket.DoubleElimination"
        [ structureTests
        , dropTests
        , playOutTests
        , grandFinalTests
        , undoTests
        , removeTests
        , addPlayerTests
        , pathTests
        , serializerTests
        ]


sizes : List Int
sizes =
    List.range 3 17


structureTests : Test
structureTests =
    describe "structure"
        [ describe "is a valid graph" <|
            List.concatMap
                (\n ->
                    [ test (String.fromInt n ++ " players with reset") <| \_ -> expectValidStructure (start withReset n)
                    , test (String.fromInt n ++ " players without reset") <| \_ -> expectValidStructure (start noReset n)
                    , test (String.fromInt n ++ " players seeded") <|
                        \_ -> expectValidStructure (DE.startSeeded withReset (players n))
                    ]
                )
                sizes
        , describe "has 2n - 2 matches and the grand final, plus the reset match" <|
            List.map
                (\n ->
                    test (String.fromInt n) <|
                        \_ ->
                            ( List.length (keyed (start withReset n)), List.length (keyed (start noReset n)) )
                                |> Expect.equal ( 2 * n - 1, 2 * n - 2 )
                )
                sizes
        , describe "every winner bracket match sends its loser to the loser bracket" <|
            List.map
                (\n ->
                    test (String.fromInt n) <|
                        \_ ->
                            matchesIn DE.isWinnerRound (start withReset n)
                                |> List.filter (\( _, m ) -> Maybe.map (Tuple.first >> DE.isLoserRound) m.loser /= Just True)
                                |> Expect.equal []
                )
                sizes
        , describe "only the loser bracket final has a loser target, the 3rd place" <|
            List.map
                (\n ->
                    test (String.fromInt n) <|
                        \_ ->
                            matchesIn DE.isLoserRound (start withReset n)
                                |> List.filterMap (\( key, m ) -> Maybe.map (Tuple.pair key) m.loser)
                                |> Expect.equal [ ( ( DE.loserRound 0, 0 ), ( -1, 2 ) ) ]
                )
                sizes
        , test "3 players: one loser bracket match between the losers of both winner bracket matches" <|
            \_ ->
                matchesIn DE.isLoserRound (start withReset 3)
                    |> List.map (\( key, m ) -> ( key, m.player1, m.player2 ))
                    |> Expect.equal [ ( ( -2, 0 ), ( -2, 0 ), ( -2, 1 ) ) ]
        , test "8 players: 4 rounds in the loser bracket with 2, 2, 1 and 1 matches" <|
            \_ ->
                start withReset 8
                    |> .rounds
                    |> Dict.filter (\round _ -> DE.isLoserRound round)
                    |> Dict.toList
                    |> List.map (Tuple.mapSecond Dict.size)
                    |> Expect.equal [ ( -5, 2 ), ( -4, 2 ), ( -3, 1 ), ( -2, 1 ) ]
        , test "the winner bracket final sends its winner to the grand final" <|
            \_ ->
                Knockout.matchAt ( 1, 0 ) (start withReset 8)
                    |> Maybe.andThen .winner
                    |> Expect.equal (Just ( 0, 0 ))
        , test "the loser bracket final sends its winner to the grand final" <|
            \_ ->
                Knockout.matchAt ( -2, 0 ) (start withReset 8)
                    |> Maybe.andThen .winner
                    |> Expect.equal (Just ( 0, 1 ))
        , test "the winner bracket is a knockout bracket" <|
            \_ ->
                start withReset 6
                    |> .slots
                    |> Dict.toList
                    |> List.map (Tuple.mapFirst (Tuple.mapFirst (\r -> r - 1)))
                    |> Expect.equal (Dict.toList (Knockout.start (players 6)).slots)
        , test "2 players meet again in the grand final" <|
            \_ ->
                start withReset 2
                    |> loserTarget ( 1, 0 )
                    |> Expect.equal (Just ( 0, 1 ))
        , test "fewer than 2 players have no matches" <|
            \_ -> start withReset 1 |> keyed |> Expect.equal []
        ]


dropTests : Test
dropTests =
    describe "drop positions"
        [ test "8 players: the first round losers meet in pairs" <|
            \_ ->
                List.map (\i -> loserTarget ( 3, i ) (start withReset 8)) [ 0, 1, 2, 3 ]
                    |> Expect.equal [ Just ( -5, 0 ), Just ( -5, 1 ), Just ( -5, 2 ), Just ( -5, 3 ) ]
        , test "8 players: the second round losers drop in reversed order" <|
            \_ ->
                List.map (\i -> loserTarget ( 2, i ) (start withReset 8)) [ 0, 1 ]
                    |> Expect.equal [ Just ( -4, 3 ), Just ( -4, 1 ) ]
        , test "8 players: the winner bracket final loser meets the loser bracket winner" <|
            \_ ->
                loserTarget ( 1, 0 ) (start withReset 8)
                    |> Expect.equal (Just ( -2, 1 ))
        , test "16 players: the second round losers drop in reversed order" <|
            \_ ->
                List.map (\i -> loserTarget ( 3, i ) (start withReset 16)) [ 0, 1, 2, 3 ]
                    |> Expect.equal [ Just ( -6, 7 ), Just ( -6, 5 ), Just ( -6, 3 ), Just ( -6, 1 ) ]
        , test "16 players: the semi-final losers drop with swapped halves" <|
            \_ ->
                List.map (\i -> loserTarget ( 2, i ) (start withReset 16)) [ 0, 1 ]
                    |> Expect.equal [ Just ( -4, 3 ), Just ( -4, 1 ) ]
        , test "32 players: the third drop round is swapped by halves" <|
            \_ ->
                List.map (DE.dropIndex 2 4) [ 0, 1, 2, 3 ]
                    |> Expect.equal [ 2, 3, 0, 1 ]
        , test "5 players: the pre-round loser waits for a second round loser" <|
            \_ ->
                start withReset 5
                    |> loserTarget ( 3, 0 )
                    |> Expect.equal (Just ( -4, 0 ))
        , test "5 players: a second round loser without an opponent moves on directly" <|
            \_ ->
                List.map (\i -> loserTarget ( 2, i ) (start withReset 5)) [ 0, 1 ]
                    |> Expect.equal [ Just ( -3, 1 ), Just ( -4, 1 ) ]
        , describe "a second round loser doesn't meet a player again in their first loser bracket match" <|
            List.concatMap
                (\n ->
                    List.map
                        (\( name, strategy ) ->
                            test (String.fromInt n ++ ", " ++ name) <|
                                \_ ->
                                    let
                                        game =
                                            start withReset n

                                        secondRound =
                                            Dict.keys game.rounds |> List.maximum |> Maybe.withDefault 0 |> (\r -> r - 1)

                                        ( final, played ) =
                                            playOut withReset strategy game

                                        dropMatches =
                                            matchesIn ((==) secondRound) final
                                                |> List.filterMap (\( _, m ) -> m.loser)
                                                |> List.filterMap (\slot -> keyed final |> List.Extra.find (\( _, m ) -> m.player1 == slot || m.player2 == slot))
                                                |> List.filterMap (\( key, m ) -> Knockout.opponents m final |> Maybe.map (Tuple.pair key))

                                        earlier key =
                                            played
                                                |> List.reverse
                                                |> List.Extra.dropWhile (Tuple.first >> (/=) key)
                                                |> List.drop 1
                                                |> List.map Tuple.second

                                        met ( a, b ) pairs =
                                            List.any (\( w, l ) -> (w == a && l == b) || (w == b && l == a)) pairs
                                    in
                                    dropMatches
                                        |> List.filter (\( key, pair ) -> met pair (earlier key))
                                        |> Expect.equal []
                        )
                        strategies
                )
                (List.range 7 17)
        ]


playOutTests : Test
playOutTests =
    describe "playing to the end"
        (List.concatMap
            (\n ->
                List.concatMap
                    (\( settingsName, settings ) ->
                        List.map
                            (\( name, strategy ) ->
                                test (String.fromInt n ++ " players, " ++ settingsName ++ ", " ++ name) <|
                                    \_ ->
                                        let
                                            ( final, played ) =
                                                playOut settings strategy (DE.start settings (players n))

                                            places =
                                                podium final

                                            placed =
                                                List.concatMap Tuple.second places

                                            lost =
                                                losses played

                                            champion =
                                                List.head placed |> Maybe.withDefault -1

                                            second =
                                                List.drop 1 placed |> List.head |> Maybe.withDefault -1
                                        in
                                        Expect.all
                                            [ \_ -> List.map (Tuple.second >> List.length) places |> Expect.equal [ 1, 1, 1 ]
                                            , \_ -> Set.size (Set.fromList placed) |> Expect.equal 3
                                            , \_ ->
                                                Dict.get champion lost
                                                    |> Maybe.withDefault 0
                                                    |> Expect.atMost 1
                                            , \_ ->
                                                players n
                                                    |> List.filter (\id -> id /= champion && (settings.reset || id /= second))
                                                    |> List.filter (\id -> Dict.get id lost /= Just 2)
                                                    |> Expect.equal []
                                                    |> Expect.onFail "everybody else lost twice"
                                            , \_ ->
                                                players n
                                                    |> List.filter (\id -> DE.isEliminated id final)
                                                    |> List.length
                                                    |> Expect.equal (n - 3)
                                                    |> Expect.onFail "all but the placed players are out"
                                            ]
                                            ()
                            )
                            strategies
                    )
                    [ ( "reset", withReset ), ( "no reset", noReset ) ]
            )
            sizes
        )


{-| 4 players: the winner bracket goes to player 0, the loser bracket to 2.
-}
beforeGrandFinal : Game
beforeGrandFinal =
    start withReset 4
        |> win False ( 2, 0 )
        |> win False ( 2, 1 )
        |> win False ( 1, 0 )
        |> win False ( -3, 0 )
        |> win True ( -2, 0 )


grandFinalTests : Test
grandFinalTests =
    describe "grand final"
        [ test "the winner bracket winner meets the loser bracket winner" <|
            \_ ->
                Knockout.matchAt DE.grandFinal beforeGrandFinal
                    |> Maybe.andThen (\m -> Knockout.opponents m beforeGrandFinal)
                    |> Expect.equal (Just ( 0, 2 ))
        , test "the loser of the loser bracket final is 3rd" <|
            \_ ->
                podium beforeGrandFinal
                    |> Expect.equal [ ( 3, [ 1 ] ) ]
        , test "the reset match can't be played before the grand final" <|
            \_ ->
                beforeGrandFinal
                    |> win False DE.resetMatch
                    |> Expect.equal beforeGrandFinal
        , test "a win of the winner bracket winner decides the tournament" <|
            \_ ->
                beforeGrandFinal
                    |> win False DE.grandFinal
                    |> Expect.all
                        [ podium >> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 2 ] ), ( 3, [ 1 ] ) ]
                        , DE.isResetSkipped >> Expect.equal True
                        , DE.playable DE.resetMatch >> Expect.equal False
                        ]
        , test "the skipped reset match takes no games" <|
            \_ ->
                let
                    decided =
                        win False DE.grandFinal beforeGrandFinal
                in
                decided
                    |> win True DE.resetMatch
                    |> Expect.equal decided
        , test "a win of the loser bracket winner leads to the reset match" <|
            \_ ->
                beforeGrandFinal
                    |> win True DE.grandFinal
                    |> Expect.all
                        [ podium >> Expect.equal [ ( 3, [ 1 ] ) ]
                        , DE.resetNeeded >> Expect.equal True
                        , \game ->
                            Knockout.matchAt DE.resetMatch game
                                |> Maybe.andThen (\m -> Knockout.opponents m game)
                                |> Expect.equal (Just ( 2, 0 ))
                        , \game -> List.map (\id -> DE.isEliminated id game) [ 0, 2 ] |> Expect.equal [ False, False ]
                        ]
        , test "the reset match decides the places" <|
            \_ ->
                beforeGrandFinal
                    |> win True DE.grandFinal
                    |> win True DE.resetMatch
                    |> podium
                    |> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 2 ] ), ( 3, [ 1 ] ) ]
        , test "without reset the grand final decides the places" <|
            \_ ->
                start noReset 4
                    |> DE.update noReset (WinGame False ( 2, 0 ))
                    |> DE.update noReset (WinGame False ( 2, 1 ))
                    |> DE.update noReset (WinGame False ( 1, 0 ))
                    |> DE.update noReset (WinGame False ( -3, 0 ))
                    |> DE.update noReset (WinGame True ( -2, 0 ))
                    |> DE.update noReset (WinGame True DE.grandFinal)
                    |> podium
                    |> Expect.equal [ ( 1, [ 2 ] ), ( 2, [ 0 ] ), ( 3, [ 1 ] ) ]
        , test "the final length applies to the grand final and the reset match" <|
            \_ ->
                let
                    settings =
                        { withReset | bestOf = 3, finalBestOf = Just 5 }
                in
                List.map (DE.matchLength settings) [ ( 1, 0 ), ( -2, 0 ), DE.grandFinal, DE.resetMatch ]
                    |> Expect.equal [ 3, 3, 5, 5 ]
        ]


undoTests : Test
undoTests =
    describe "undo"
        [ test "a winner bracket match can be undone while its loser's match has no result" <|
            \_ ->
                start withReset 4
                    |> win False ( 2, 0 )
                    |> undo ( 2, 0 )
                    |> Expect.equal (start withReset 4)
        , test "a winner bracket match can't be undone once its loser played in the loser bracket" <|
            \_ ->
                let
                    played =
                        start withReset 4
                            |> win False ( 2, 0 )
                            |> win False ( 2, 1 )
                            |> win False ( -3, 0 )
                in
                played
                    |> undo ( 2, 0 )
                    |> Expect.equal played
        , test "a winner bracket match can't be undone once its winner played" <|
            \_ ->
                let
                    played =
                        start withReset 4
                            |> win False ( 2, 0 )
                            |> win False ( 2, 1 )
                            |> win False ( 1, 0 )
                in
                played
                    |> undo ( 2, 0 )
                    |> Expect.equal played
        , test "undoing a loser bracket match brings its loser back" <|
            \_ ->
                start withReset 4
                    |> win False ( 2, 0 )
                    |> win False ( 2, 1 )
                    |> win False ( -3, 0 )
                    |> undo ( -3, 0 )
                    |> Expect.all
                        [ DE.isEliminated 3 >> Expect.equal False
                        , .slots >> Dict.get ( -2, 0 ) >> Expect.equal Nothing
                        ]
        , test "the grand final can be undone after the winner bracket winner won it" <|
            \_ ->
                beforeGrandFinal
                    |> win False DE.grandFinal
                    |> undo DE.grandFinal
                    |> Expect.equal beforeGrandFinal
        , test "the grand final can be undone while the reset match has no result" <|
            \_ ->
                beforeGrandFinal
                    |> win True DE.grandFinal
                    |> undo DE.grandFinal
                    |> Expect.equal beforeGrandFinal
        , test "the grand final can't be undone once the reset match has a result" <|
            \_ ->
                let
                    played =
                        beforeGrandFinal
                            |> win True DE.grandFinal
                            |> win False DE.resetMatch
                in
                played
                    |> undo DE.grandFinal
                    |> Expect.equal played
        ]


removeTests : Test
removeTests =
    describe "removePlayer"
        [ test "the opponent in the winner bracket wins, the removed player is out" <|
            \_ ->
                start withReset 4
                    |> DE.removePlayer 1
                    |> Expect.all
                        [ .slots >> Dict.get ( 1, 0 ) >> Expect.equal (Just 0)
                        , DE.isEliminated 1 >> Expect.equal True
                        ]
        , test "the removed player loses their loser bracket match too" <|
            \_ ->
                start withReset 4
                    |> DE.removePlayer 1
                    |> win False ( 2, 1 )
                    |> .slots
                    |> Dict.get ( -2, 0 )
                    |> Expect.equal (Just 3)
        , test "a player removed in the loser bracket gives the opponent a walkover" <|
            \_ ->
                start withReset 4
                    |> win False ( 2, 0 )
                    |> win False ( 2, 1 )
                    |> DE.removePlayer 3
                    |> .slots
                    |> Dict.get ( -2, 0 )
                    |> Expect.equal (Just 1)
        , test "two removed players in the loser bracket pass a walkover on" <|
            \_ ->
                start withReset 4
                    |> DE.removePlayer 1
                    |> DE.removePlayer 3
                    |> Expect.all
                        [ \game ->
                            Knockout.matchAt ( -3, 0 ) game
                                |> Maybe.map (\m -> Knockout.isDecided m game)
                                |> Expect.equal (Just True)
                        , win False ( 1, 0 )
                            >> .slots
                            >> Dict.get ( 0, 1 )
                            >> Expect.equal (Just 2)
                        ]
        , test "the winner bracket winner removed before the grand final: the loser bracket winner wins" <|
            \_ ->
                beforeGrandFinal
                    |> DE.removePlayer 0
                    |> podium
                    |> Expect.equal [ ( 1, [ 2 ] ), ( 3, [ 1 ] ) ]
        , test "the loser bracket winner removed before the grand final: the reset match isn't needed" <|
            \_ ->
                beforeGrandFinal
                    |> DE.removePlayer 2
                    |> podium
                    |> Expect.equal [ ( 1, [ 0 ] ), ( 3, [ 1 ] ) ]
        , test "removing the champion after a skipped reset keeps the places" <|
            \_ ->
                beforeGrandFinal
                    |> win False DE.grandFinal
                    |> DE.removePlayer 0
                    |> podium
                    |> Expect.equal [ ( 1, [ 0 ] ), ( 2, [ 2 ] ), ( 3, [ 1 ] ) ]
        , test "a removed player can't be brought back by undo" <|
            \_ ->
                let
                    removed =
                        start withReset 4 |> DE.removePlayer 1
                in
                removed
                    |> undo ( 2, 0 )
                    |> Expect.equal removed
        ]


addPlayerTests : Test
addPlayerTests =
    describe "addPlayer"
        [ test "before the first result the brackets grow" <|
            \_ ->
                start withReset 4
                    |> DE.addPlayer withReset 4
                    |> Expect.all
                        [ keyed >> List.length >> Expect.equal 9
                        , expectValidStructure
                        , .slots >> Dict.values >> List.sort >> Expect.equal [ 0, 1, 2, 3, 4 ]
                        ]
        , test "growing keeps the places of the players in the winner bracket" <|
            \_ ->
                start withReset 4
                    |> DE.addPlayer withReset 4
                    |> .slots
                    |> Dict.toList
                    |> List.map (Tuple.mapFirst (Tuple.mapFirst (\r -> r - 1)))
                    |> Expect.equal
                        (Knockout.start (players 4) |> Knockout.addPlayer 4 |> .slots |> Dict.toList)
        , describe "a grown bracket is the same as one started with all players" <|
            List.map
                (\n ->
                    test (String.fromInt n) <|
                        \_ ->
                            List.foldl (DE.addPlayer withReset) (start withReset 3) (List.range 3 (n - 1))
                                |> keyed
                                |> List.length
                                |> Expect.equal (List.length (keyed (start withReset n)))
                )
                sizes
        , test "after the first result nobody joins" <|
            \_ ->
                let
                    played =
                        start withReset 4 |> win False ( 2, 0 )
                in
                ( DE.acceptsPlayers played, DE.addPlayer withReset 4 played == played )
                    |> Expect.equal ( False, True )
        , test "a new tournament accepts players" <|
            \_ -> DE.acceptsPlayers (start withReset 4) |> Expect.equal True
        ]


pathTests : Test
pathTests =
    describe "playerPath"
        [ test "the path leads through both brackets with the drop line" <|
            \_ ->
                start withReset 4
                    |> win True ( 2, 0 )
                    |> win False ( 2, 1 )
                    |> win False ( -3, 0 )
                    |> Knockout.playerPath 0
                    |> Expect.equal
                        { slots = Set.fromList [ ( 2, 0 ), ( -3, 0 ), ( -2, 0 ) ]
                        , matches = Set.fromList [ ( 2, 0 ), ( -3, 0 ), ( -2, 0 ) ]
                        , lines = Set.fromList [ ( -3, 0 ), ( -2, 0 ) ]
                        }
        ]


serializerTests : Test
serializerTests =
    describe "Serializer"
        [ test "settings round trip" <|
            \_ ->
                let
                    settings =
                        { reset = False, bestOf = 3, finalBestOf = Just 5 }
                in
                Serializer.encodeSettings settings
                    |> JD.decodeValue Serializer.decodeSettings
                    |> Expect.equal (Ok settings)
        , test "game round trip" <|
            \_ ->
                Serializer.encodeGame beforeGrandFinal
                    |> JD.decodeValue Serializer.decodeGame
                    |> Expect.equal (Ok beforeGrandFinal)
        ]
