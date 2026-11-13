module ModeSelectorTest exposing (suite)

import Bracket.Knockout exposing (ThirdPlace(..))
import Expect
import ModeSelector exposing (Msg(..))
import Test exposing (Test, describe, test)
import Tournament exposing (Mode(..), ModeSettings(..), Settings)


knockout : ThirdPlace -> Settings
knockout thirdPlace =
    { shuffle = False
    , mode = KnockoutSettings { thirdPlace = thirdPlace }
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
                Tournament.modeDefaults KnockoutMode
                    |> Tournament.modeOf
                    |> Expect.equal KnockoutMode
        , test "the default settings are a knockout with a playoff and shuffle" <|
            \_ ->
                Tournament.defaultSettings
                    |> Expect.equal { shuffle = True, mode = KnockoutSettings { thirdPlace = Playoff } }
        , test "changing the mode settings keeps shuffle" <|
            \_ ->
                settingsAfter (SetModeSettings <| KnockoutSettings { thirdPlace = NoThirdPlace }) (knockout Playoff)
                    |> Expect.equal (knockout NoThirdPlace)
        , test "changing shuffle keeps the mode settings" <|
            \_ ->
                settingsAfter (SetShuffle True) (knockout Shared)
                    |> Expect.equal { shuffle = True, mode = KnockoutSettings { thirdPlace = Shared } }
        ]
