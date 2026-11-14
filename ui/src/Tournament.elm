module Tournament exposing
    ( Local
    , Mode(..)
    , ModeSettings(..)
    , Msg(..)
    , Order(..)
    , Settings
    , Tournament(..)
    , addPlayer
    , controls
    , csv
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
import Bracket.RoundRobin.Csv
import Bracket.RoundRobin.Serializer
import Bracket.RoundRobin.View
import Bracket.Swiss
import Bracket.Swiss.Csv
import Bracket.Swiss.Serializer
import Csv
import Dict exposing (Dict)
import GameBox
import Html exposing (Html)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE
import Placement exposing (Placement)
import PlayerId exposing (PlayerId)


{-| The settings for the next start. A running tournament keeps its own copy
of the mode settings.
-}
type alias Settings =
    { order : Order
    , mode : ModeSettings
    }


{-| How the players are ordered at the start. `Seeded` uses the player order of
the editor, the best player first.
-}
type Order
    = Shuffled
    | Seeded


type ModeSettings
    = KnockoutSettings Bracket.Knockout.Settings
    | RoundRobinSettings Bracket.RoundRobin.Settings
    | SwissSettings Bracket.Swiss.Settings


{-| Identifies a mode without its settings.
-}
type Mode
    = KnockoutMode
    | RoundRobinMode
    | SwissMode


type Tournament
    = Knockout Bracket.Knockout.State
    | RoundRobin Bracket.RoundRobin.State
    | Swiss Bracket.Swiss.State


type Msg
    = KnockoutMsg Bracket.Knockout.Msg
    | RoundRobinMsg Bracket.RoundRobin.View.Msg
    | SwissMsg Bracket.Swiss.Msg


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
    { order = Shuffled
    , mode = modeDefaults KnockoutMode
    }


modeOf : ModeSettings -> Mode
modeOf settings =
    case settings of
        KnockoutSettings _ ->
            KnockoutMode

        RoundRobinSettings _ ->
            RoundRobinMode

        SwissSettings _ ->
            SwissMode


modeDefaults : Mode -> ModeSettings
modeDefaults mode =
    case mode of
        KnockoutMode ->
            KnockoutSettings Bracket.Knockout.defaultSettings

        RoundRobinMode ->
            RoundRobinSettings Bracket.RoundRobin.defaultSettings

        SwissMode ->
            SwissSettings Bracket.Swiss.defaultSettings


minPlayers : ModeSettings -> Int
minPlayers mode =
    case mode of
        KnockoutSettings _ ->
            2

        RoundRobinSettings _ ->
            2

        SwissSettings _ ->
            2


{-| Starts a tournament with the players in list order. A seeded knockout
places them by their seeds.
-}
start : Settings -> List PlayerId -> Tournament
start settings players =
    case settings.mode of
        KnockoutSettings knockout ->
            Knockout
                { settings = knockout
                , game =
                    (case settings.order of
                        Shuffled ->
                            Bracket.Knockout.start players

                        Seeded ->
                            Bracket.Knockout.startSeeded players
                    )
                        |> Bracket.Knockout.Placement.apply knockout
                }

        RoundRobinSettings roundRobin ->
            RoundRobin <| Bracket.RoundRobin.start roundRobin players

        SwissSettings swiss ->
            Swiss <| Bracket.Swiss.start swiss players


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

        Swiss state ->
            Swiss <| Bracket.Swiss.addPlayer id state


removePlayer : PlayerId -> Tournament -> Tournament
removePlayer id tournament =
    case tournament of
        Knockout state ->
            mapKnockoutGame (Bracket.Knockout.removePlayer id) state

        RoundRobin state ->
            RoundRobin <| Bracket.RoundRobin.removePlayer id state

        Swiss state ->
            Swiss <| Bracket.Swiss.removePlayer id state


{-| Whether the player takes part and is out. In a round robin and a Swiss
tournament only removed players are out.
-}
isEliminated : PlayerId -> Tournament -> Bool
isEliminated id tournament =
    case tournament of
        Knockout state ->
            Bracket.Knockout.isEliminated id state.game

        RoundRobin state ->
            Bracket.RoundRobin.isRemoved id state.game

        Swiss state ->
            Bracket.Swiss.isRemoved id state.game


update : Msg -> Local -> Tournament -> ( Local, Tournament )
update msg local tournament =
    case ( msg, tournament ) of
        ( KnockoutMsg sub, Knockout state ) ->
            ( local, mapKnockoutGame (Bracket.Knockout.update state.settings sub) state )

        ( RoundRobinMsg sub, RoundRobin state ) ->
            Bracket.RoundRobin.View.update sub local.roundRobin state
                |> Tuple.mapBoth (\roundRobin -> { local | roundRobin = roundRobin }) RoundRobin

        ( SwissMsg sub, Swiss state ) ->
            ( local, Swiss <| Bracket.Swiss.update sub state )

        _ ->
            ( local, tournament )


controls : Tournament -> GameBox.Controls
controls tournament =
    case tournament of
        Knockout _ ->
            { hideFinished = True, headers = False }

        RoundRobin _ ->
            { hideFinished = False, headers = True }

        Swiss _ ->
            { hideFinished = False, headers = False }


{-| Only editors can change results.
-}
view : { names : Dict PlayerId String, box : GameBox.Model, canEdit : Bool } -> Local -> Tournament -> Html Msg
view config local tournament =
    case tournament of
        Knockout state ->
            Html.map KnockoutMsg <|
                Bracket.Knockout.viewGame
                    { names = config.names
                    , canEdit = config.canEdit
                    , hideFinished = config.box.hide
                    , settings = state.settings
                    }
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

        Swiss _ ->
            Html.text ""


{-| The tournament as CSV, if the mode offers it.
-}
csv : Tournament -> Maybe (Dict PlayerId String -> String)
csv tournament =
    case tournament of
        Knockout _ ->
            Nothing

        RoundRobin state ->
            Just <| \names -> Csv.encode <| Bracket.RoundRobin.Csv.table names state

        Swiss state ->
            Just <| \names -> Csv.encode <| Bracket.Swiss.Csv.table names state


placements : Tournament -> List Placement
placements tournament =
    case tournament of
        Knockout state ->
            Bracket.Knockout.Placement.placements state.game

        RoundRobin state ->
            Bracket.RoundRobin.placements state

        Swiss state ->
            Bracket.Swiss.placements state


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


{-| Up to version 1 of the data format a `"shuffle"` flag stood for the order.
Without shuffle the players started in the order they were added, and the
seeded order starts the same way.
-}
decodeSettings : Int -> Decoder Settings
decodeSettings version =
    JD.succeed Settings
        |> (if version < 2 then
                required "shuffle" <|
                    JD.map
                        (\shuffle ->
                            if shuffle then
                                Shuffled

                            else
                                Seeded
                        )
                        JD.bool

            else
                required "order" decodeOrder
           )
        |> required "mode"
            (decodeMode <|
                \mode ->
                    case mode of
                        "knockout" ->
                            JD.field "settings" (Bracket.Knockout.Serializer.decodeSettings version)
                                |> JD.map KnockoutSettings

                        "roundRobin" ->
                            JD.field "settings" Bracket.RoundRobin.Serializer.decodeSettings
                                |> JD.map RoundRobinSettings

                        "swiss" ->
                            JD.field "settings" Bracket.Swiss.Serializer.decodeSettings
                                |> JD.map SwissSettings

                        _ ->
                            unknownMode mode
            )


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "order", encodeOrder settings.order )
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

                SwissSettings swiss ->
                    JE.object
                        [ ( "mode", JE.string "swiss" )
                        , ( "settings", Bracket.Swiss.Serializer.encodeSettings swiss )
                        ]
          )
        ]


decodeOrder : Decoder Order
decodeOrder =
    JD.string
        |> JD.andThen
            (\order ->
                case order of
                    "random" ->
                        JD.succeed Shuffled

                    "seeded" ->
                        JD.succeed Seeded

                    _ ->
                        JD.fail <| "unknown order: " ++ order
            )


encodeOrder : Order -> Value
encodeOrder order =
    JE.string <|
        case order of
            Shuffled ->
                "random"

            Seeded ->
                "seeded"


decodeTournament : Int -> Decoder Tournament
decodeTournament version =
    decodeMode <|
        \mode ->
            case mode of
                "knockout" ->
                    JD.succeed Bracket.Knockout.State
                        |> required "settings" (Bracket.Knockout.Serializer.decodeSettings version)
                        |> required "game" (Bracket.Knockout.Serializer.decodeGame version)
                        |> JD.map Knockout

                "roundRobin" ->
                    JD.succeed Bracket.RoundRobin.State
                        |> required "settings" Bracket.RoundRobin.Serializer.decodeSettings
                        |> required "game" Bracket.RoundRobin.Serializer.decodeGame
                        |> JD.map RoundRobin

                "swiss" ->
                    JD.succeed Bracket.Swiss.State
                        |> required "settings" Bracket.Swiss.Serializer.decodeSettings
                        |> required "game" Bracket.Swiss.Serializer.decodeGame
                        |> JD.map Swiss

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

        Swiss state ->
            JE.object
                [ ( "mode", JE.string "swiss" )
                , ( "settings", Bracket.Swiss.Serializer.encodeSettings state.settings )
                , ( "game", Bracket.Swiss.Serializer.encodeGame state.game )
                ]
