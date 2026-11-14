module Tournament exposing
    ( Local
    , Mode(..)
    , ModeSettings(..)
    , Msg(..)
    , Settings
    , Tournament(..)
    , addPlayer
    , controls
    , decodeSettings
    , decodeTournament
    , defaultSettings
    , encodeSettings
    , encodeTournament
    , initLocal
    , isEliminated
    , minPlayers
    , modeDefaults
    , modeOf
    , placements
    , removePlayer
    , start
    , update
    , view
    , viewPlacements
    )

{-| The interface between the app and the tournament modes. Every function
dispatches to the module of the mode.
-}

import Bracket.Knockout
import Bracket.Knockout.Placement
import Bracket.Knockout.Serializer
import Bracket.RoundRobin
import Bracket.RoundRobin.Serializer
import Bracket.RoundRobin.View
import Dict exposing (Dict)
import GameBox
import Html exposing (Html)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE
import Placement exposing (Placement)
import PlayerId exposing (PlayerId)


{-| The settings for the next start. A running tournament keeps its own copy.
-}
type alias Settings =
    { shuffle : Bool
    , mode : ModeSettings
    }


type ModeSettings
    = KnockoutSettings Bracket.Knockout.Settings
    | RoundRobinSettings Bracket.RoundRobin.Settings


{-| Identifies a mode without its settings.
-}
type Mode
    = KnockoutMode
    | RoundRobinMode


type Tournament
    = Knockout Bracket.Knockout.State
    | RoundRobin Bracket.RoundRobin.State


type Msg
    = KnockoutMsg Bracket.Knockout.Msg
    | RoundRobinMsg Bracket.RoundRobin.View.Msg


{-| The view state of one viewer. It isn't shared.
-}
type alias Local =
    { roundRobin : Bracket.RoundRobin.View.Model
    }


initLocal : Local
initLocal =
    { roundRobin = Bracket.RoundRobin.View.init }


defaultSettings : Settings
defaultSettings =
    { shuffle = True
    , mode = modeDefaults KnockoutMode
    }


modeOf : ModeSettings -> Mode
modeOf settings =
    case settings of
        KnockoutSettings _ ->
            KnockoutMode

        RoundRobinSettings _ ->
            RoundRobinMode


modeDefaults : Mode -> ModeSettings
modeDefaults mode =
    case mode of
        KnockoutMode ->
            KnockoutSettings Bracket.Knockout.defaultSettings

        RoundRobinMode ->
            RoundRobinSettings Bracket.RoundRobin.defaultSettings


minPlayers : ModeSettings -> Int
minPlayers mode =
    case mode of
        KnockoutSettings _ ->
            2

        RoundRobinSettings _ ->
            2


{-| Starts a tournament with the players in list order.
-}
start : Settings -> List PlayerId -> Tournament
start settings players =
    case settings.mode of
        KnockoutSettings knockout ->
            Knockout
                { settings = knockout
                , game =
                    Bracket.Knockout.start players
                        |> Bracket.Knockout.Placement.apply knockout
                }

        RoundRobinSettings roundRobin ->
            RoundRobin <| Bracket.RoundRobin.start roundRobin players


mapKnockoutGame : (Bracket.Knockout.Game -> Bracket.Knockout.Game) -> Bracket.Knockout.State -> Tournament
mapKnockoutGame mapper state =
    Knockout { state | game = mapper state.game }


addPlayer : PlayerId -> Tournament -> Tournament
addPlayer id tournament =
    case tournament of
        Knockout state ->
            mapKnockoutGame
                (Bracket.Knockout.addPlayer id
                    >> Bracket.Knockout.Placement.apply state.settings
                )
                state

        RoundRobin state ->
            RoundRobin <| Bracket.RoundRobin.addPlayer id state


removePlayer : PlayerId -> Tournament -> Tournament
removePlayer id tournament =
    case tournament of
        Knockout state ->
            mapKnockoutGame (Bracket.Knockout.removePlayer id) state

        RoundRobin state ->
            RoundRobin <| Bracket.RoundRobin.removePlayer id state


{-| Whether the player takes part and is out. In a round robin only removed
players are out.
-}
isEliminated : PlayerId -> Tournament -> Bool
isEliminated id tournament =
    case tournament of
        Knockout state ->
            Bracket.Knockout.isEliminated id state.game

        RoundRobin state ->
            Bracket.RoundRobin.isRemoved id state.game


update : Msg -> Local -> Tournament -> ( Local, Tournament )
update msg local tournament =
    case ( msg, tournament ) of
        ( KnockoutMsg sub, Knockout state ) ->
            ( local, mapKnockoutGame (Bracket.Knockout.update sub) state )

        ( RoundRobinMsg sub, RoundRobin state ) ->
            Bracket.RoundRobin.View.update sub local.roundRobin state
                |> Tuple.mapBoth (\roundRobin -> { local | roundRobin = roundRobin }) RoundRobin

        _ ->
            ( local, tournament )


controls : Tournament -> GameBox.Controls
controls tournament =
    case tournament of
        Knockout _ ->
            { hideFinished = True, headers = False }

        RoundRobin _ ->
            { hideFinished = False, headers = True }


{-| Only editors can change results.
-}
view : { names : Dict PlayerId String, box : GameBox.Model, canEdit : Bool } -> Local -> Tournament -> Html Msg
view config local tournament =
    case tournament of
        Knockout state ->
            Html.map KnockoutMsg <|
                Bracket.Knockout.viewGame { names = config.names, canEdit = config.canEdit } <|
                    if config.box.hide then
                        Bracket.Knockout.hideFinishedMatches state.game

                    else
                        state.game

        RoundRobin state ->
            Html.map RoundRobinMsg <|
                Bracket.RoundRobin.View.view
                    { names = config.names
                    , canEdit = config.canEdit
                    , headers = config.box.headers
                    }
                    local.roundRobin
                    state


placements : Tournament -> List Placement
placements tournament =
    case tournament of
        Knockout state ->
            Bracket.Knockout.Placement.placements state.game

        RoundRobin state ->
            Bracket.RoundRobin.placements state


viewPlacements : Dict PlayerId String -> Tournament -> Html msg
viewPlacements names tournament =
    Placement.view
        { names = names
        , isEliminated = \id -> isEliminated id tournament
        }
        (placements tournament)


{-| Every mode is an object with a `"mode"` field.
-}
decodeMode : (String -> Decoder a) -> Decoder a
decodeMode decoder =
    JD.field "mode" JD.string
        |> JD.andThen decoder


unknownMode : String -> Decoder a
unknownMode mode =
    JD.fail <| "unknown mode: " ++ mode


decodeSettings : Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> required "shuffle" JD.bool
        |> required "mode"
            (decodeMode <|
                \mode ->
                    case mode of
                        "knockout" ->
                            JD.field "settings" Bracket.Knockout.Serializer.decodeSettings
                                |> JD.map KnockoutSettings

                        "roundRobin" ->
                            JD.field "settings" Bracket.RoundRobin.Serializer.decodeSettings
                                |> JD.map RoundRobinSettings

                        _ ->
                            unknownMode mode
            )


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "shuffle", JE.bool settings.shuffle )
        , ( "mode"
          , case settings.mode of
                KnockoutSettings knockout ->
                    JE.object
                        [ ( "mode", JE.string "knockout" )
                        , ( "settings", Bracket.Knockout.Serializer.encodeSettings knockout )
                        ]

                RoundRobinSettings roundRobin ->
                    JE.object
                        [ ( "mode", JE.string "roundRobin" )
                        , ( "settings", Bracket.RoundRobin.Serializer.encodeSettings roundRobin )
                        ]
          )
        ]


decodeTournament : Decoder Tournament
decodeTournament =
    decodeMode <|
        \mode ->
            case mode of
                "knockout" ->
                    JD.succeed Bracket.Knockout.State
                        |> required "settings" Bracket.Knockout.Serializer.decodeSettings
                        |> required "game" Bracket.Knockout.Serializer.decodeGame
                        |> JD.map Knockout

                "roundRobin" ->
                    JD.succeed Bracket.RoundRobin.State
                        |> required "settings" Bracket.RoundRobin.Serializer.decodeSettings
                        |> required "game" Bracket.RoundRobin.Serializer.decodeGame
                        |> JD.map RoundRobin

                _ ->
                    unknownMode mode


encodeTournament : Tournament -> Value
encodeTournament tournament =
    case tournament of
        Knockout state ->
            JE.object
                [ ( "mode", JE.string "knockout" )
                , ( "settings", Bracket.Knockout.Serializer.encodeSettings state.settings )
                , ( "game", Bracket.Knockout.Serializer.encodeGame state.game )
                ]

        RoundRobin state ->
            JE.object
                [ ( "mode", JE.string "roundRobin" )
                , ( "settings", Bracket.RoundRobin.Serializer.encodeSettings state.settings )
                , ( "game", Bracket.RoundRobin.Serializer.encodeGame state.game )
                ]
