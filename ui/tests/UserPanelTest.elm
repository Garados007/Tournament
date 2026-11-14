module UserPanelTest exposing (suite)

import Bracket.Knockout exposing (ThirdPlace(..))
import Data exposing (Data)
import Dict
import Expect
import ModeSelector
import Test exposing (Test, describe, test)
import Tournament exposing (ModeSettings(..), Order(..))
import UserPanel exposing (Msg(..))


{-| Players added in the order P1, P2, P3 and then moved to P3, P1, P2.
-}
data : Order -> Data
data order =
    let
        init =
            Data.init

        settings =
            Tournament.defaultSettings
    in
    { init
        | users = Dict.fromList [ ( 3, "P3" ), ( 1, "P1" ), ( 2, "P2" ) ]
        , order = [ 3, 1, 2 ]
        , nextId = 4
        , nextSettings = { settings | order = order }
    }


{-| P2 is out and sits between P3 and P1.
-}
running : Order -> Data
running order =
    data order
        |> (\d ->
                { d
                    | order = [ 3, 2, 1 ]
                    , tournament =
                        Tournament.start d.nextSettings [ 1, 2 ]
                            |> Tournament.removePlayer 2
                            |> Just
                }
           )


update : Msg -> Data -> Data
update msg =
    UserPanel.update msg (UserPanel.init True)
        >> (\( _, result, _ ) -> result)


suite : Test
suite =
    describe "UserPanel"
        [ test "seeded players start in the order of the list" <|
            \_ ->
                update Generate (data Seeded)
                    |> .tournament
                    |> Expect.equal (Just <| Tournament.start (data Seeded).nextSettings [ 3, 1, 2 ])
        , test "with a random order the start waits for the shuffled list" <|
            \_ ->
                update Generate (data Shuffled)
                    |> .tournament
                    |> Expect.equal Nothing
        , test "players who are out leave the list at the next start" <|
            \_ ->
                update Generate (running Seeded)
                    |> Expect.all
                        [ .order >> Expect.equal [ 3, 1 ]
                        , .users >> Dict.keys >> Expect.equal [ 1, 3 ]
                        , .tournament >> Expect.equal (Just <| Tournament.start (data Seeded).nextSettings [ 3, 1 ])
                        ]
        , test "a new player is appended to the order" <|
            \_ ->
                UserPanel.update (SetInput "P4") (UserPanel.init True) (data Seeded)
                    |> (\( model, d, _ ) -> UserPanel.update AddName model d)
                    |> (\( _, result, _ ) -> result.order)
                    |> Expect.equal [ 3, 1, 2, 4 ]
        , test "moving a player up and down" <|
            \_ ->
                data Seeded
                    |> update (MovePlayer 2 -1)
                    |> Expect.all
                        [ .order >> Expect.equal [ 3, 2, 1 ]
                        , update (MovePlayer 3 1) >> .order >> Expect.equal [ 2, 3, 1 ]
                        ]
        , test "moving skips players who are out" <|
            \_ ->
                running Seeded
                    |> update (MovePlayer 3 1)
                    |> .order
                    |> Expect.equal [ 1, 2, 3 ]
        , test "removing a player without a tournament removes them from the order" <|
            \_ ->
                update (RemoveUser 1) (data Seeded)
                    |> Expect.all
                        [ .order >> Expect.equal [ 3, 2 ]
                        , .users >> Dict.keys >> Expect.equal [ 2, 3 ]
                        ]
        , test "changing the settings keeps the running tournament" <|
            \_ ->
                let
                    started =
                        data Seeded
                            |> (\d -> { d | tournament = Just <| Tournament.start d.nextSettings [ 1, 2, 3 ] })
                in
                UserPanel.update
                    (SettingsMsg <| ModeSelector.SetModeSettings <| KnockoutSettings { thirdPlace = Shared })
                    (UserPanel.init True)
                    started
                    |> (\( _, result, _ ) -> ( result.tournament, result.nextSettings.mode ))
                    |> Expect.equal ( started.tournament, KnockoutSettings { thirdPlace = Shared } )
        ]
