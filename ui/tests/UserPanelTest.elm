module UserPanelTest exposing (suite)

import Bracket.Knockout exposing (ThirdPlace(..))
import Data exposing (Data)
import Dict
import Expect
import ModeSelector
import Test exposing (Test, describe, test)
import Tournament exposing (ModeSettings(..))
import UserPanel exposing (Msg(..))


{-| Players added in the order P3, P1, P2.
-}
data : Bool -> Data
data shuffle =
    let
        init =
            Data.init

        settings =
            Tournament.defaultSettings
    in
    { init
        | users = Dict.fromList [ ( 3, "P3" ), ( 1, "P1" ), ( 2, "P2" ) ]
        , nextId = 4
        , nextSettings = { settings | shuffle = shuffle }
    }


suite : Test
suite =
    describe "UserPanel"
        [ test "without shuffle the players start in the order they were added" <|
            \_ ->
                UserPanel.update Generate (UserPanel.init True) (data False)
                    |> (\( _, result, _ ) -> result.tournament)
                    |> Expect.equal (Just <| Tournament.start (data False).nextSettings [ 1, 2, 3 ])
        , test "with shuffle the start waits for the random order" <|
            \_ ->
                UserPanel.update Generate (UserPanel.init True) (data True)
                    |> (\( _, result, _ ) -> result.tournament)
                    |> Expect.equal Nothing
        , test "changing the settings keeps the running tournament" <|
            \_ ->
                let
                    running =
                        data False
                            |> (\d -> { d | tournament = Just <| Tournament.start d.nextSettings [ 1, 2, 3 ] })
                in
                UserPanel.update
                    (SettingsMsg <| ModeSelector.SetModeSettings <| KnockoutSettings { thirdPlace = Shared })
                    (UserPanel.init True)
                    running
                    |> (\( _, result, _ ) -> ( result.tournament, result.nextSettings.mode ))
                    |> Expect.equal ( running.tournament, KnockoutSettings { thirdPlace = Shared } )
        ]
