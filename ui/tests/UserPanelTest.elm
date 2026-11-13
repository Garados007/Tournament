module UserPanelTest exposing (suite)

import Data exposing (Data)
import Dict
import Expect
import Test exposing (Test, describe, test)
import Tournament
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
        ]
