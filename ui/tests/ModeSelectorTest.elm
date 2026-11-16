module ModeSelectorTest exposing (suite)

import Bracket.Knockout exposing (Places(..))
import Bracket.RoundRobin exposing (Matches(..), Ranking(..))
import Bracket.Scoring exposing (Scoring(..))
import Expect
import Helper exposing (knockoutSettings)
import ModeSelector exposing (Msg(..))
import Test exposing (Test, describe, test)
import Tournament exposing (Mode(..), ModeSettings(..), Order(..), Settings)


knockout : Places -> Settings
knockout thirdPlace =
    { order = Seeded
    , mode = KnockoutSettings (knockoutSettings thirdPlace)
    }


settingsAfter : Msg -> Settings -> Settings
settingsAfter msg =
    ModeSelector.update msg ModeSelector.init >> Tuple.second


suite : Test
suite =
    describe "ModeSelector.update"
        [ test "showing the help keeps the settings" <|
            \_ ->
                ModeSelector.update (ShowHelp True) ModeSelector.init (knockout Shared)
                    |> Expect.equal ( { help = True }, knockout Shared )
        , test "selecting the current mode keeps its settings" <|
            \_ ->
                settingsAfter (SelectMode KnockoutMode) (knockout Shared)
                    |> Expect.equal (knockout Shared)
        , test "the defaults of every mode belong to that mode" <|
            \_ ->
                [ KnockoutMode, RoundRobinMode, SwissMode, StepladderMode, DoubleEliminationMode ]
                    |> List.map (Tournament.modeDefaults >> Tournament.modeOf)
                    |> Expect.equal [ KnockoutMode, RoundRobinMode, SwissMode, StepladderMode, DoubleEliminationMode ]
        , test "selecting another mode starts with its defaults and keeps the order" <|
            \_ ->
                settingsAfter (SelectMode RoundRobinMode) (knockout Shared)
                    |> Expect.equal
                        { order = Seeded
                        , mode = RoundRobinSettings { matches = Single, scoring = Points, ranking = ByResults }
                        }
        , test "a Swiss tournament starts with automatic rounds and points" <|
            \_ ->
                settingsAfter (SelectMode SwissMode) (knockout Shared)
                    |> Expect.equal
                        { order = Seeded
                        , mode = SwissSettings { rounds = Nothing, scoring = Points }
                        }
        , test "a stepladder starts with single games and keeps the order" <|
            \_ ->
                settingsAfter (SelectMode StepladderMode) (knockout Shared)
                    |> Expect.equal
                        { order = Seeded
                        , mode = StepladderSettings { bestOf = 1, finalBestOf = Nothing }
                        }
        , test "a double elimination starts with a reset match and single games" <|
            \_ ->
                settingsAfter (SelectMode DoubleEliminationMode) (knockout Shared)
                    |> Expect.equal
                        { order = Seeded
                        , mode = DoubleEliminationSettings { reset = True, bestOf = 1, finalBestOf = Nothing }
                        }
        , test "the default settings are a knockout with a playoff and a random order" <|
            \_ ->
                Tournament.defaultSettings
                    |> Expect.equal { order = Shuffled, mode = KnockoutSettings (knockoutSettings Playoff) }
        , test "changing the mode settings keeps the order" <|
            \_ ->
                settingsAfter (SetModeSettings <| KnockoutSettings (knockoutSettings NoThirdPlace)) (knockout Playoff)
                    |> Expect.equal (knockout NoThirdPlace)
        , test "changing the order keeps the mode settings" <|
            \_ ->
                settingsAfter (SetOrder Shuffled) (knockout Shared)
                    |> Expect.equal { order = Shuffled, mode = KnockoutSettings (knockoutSettings Shared) }
        ]
