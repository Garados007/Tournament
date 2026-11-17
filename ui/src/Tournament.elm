module Tournament exposing
    ( Entrants(..)
    , Local
    , Mode(..)
    , ModeSettings(..)
    , Msg(..)
    , Order(..)
    , Settings
    , Tournament(..)
    , acceptsPlayers
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
    , isRemoved
    , isViewerMsg
    , minPlayers
    , modeDefaults
    , modeOf
    , placements
    , removePlayer
    , start
    , takesPart
    , update
    , view
    , viewPlacements
    )

{-| The interface between the app and the tournament modes. Every function
dispatches to the module of the mode.
-}

import Bracket.DoubleElimination
import Bracket.DoubleElimination.Serializer
import Bracket.DoubleElimination.View
import Bracket.Knockout
import Bracket.Knockout.Placement
import Bracket.Knockout.Serializer
import Bracket.RoundRobin
import Bracket.RoundRobin.Csv
import Bracket.RoundRobin.Serializer
import Bracket.RoundRobin.View
import Bracket.Stepladder
import Bracket.Stepladder.Serializer
import Bracket.Stepladder.View
import Bracket.Swiss
import Bracket.Swiss.Csv
import Bracket.Swiss.Serializer
import Bracket.Swiss.View
import Csv
import Dict exposing (Dict)
import GameBox
import Html exposing (Html)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE
import Placement exposing (Placement)
import PlayerId exposing (PlayerId)


{-| The settings for the next start. A running tournament keeps its own copy
of the mode settings.
-}
type alias Settings =
    { order : Order
    , entrants : Entrants
    , mode : ModeSettings
    }


{-| How the players are ordered at the start. `Seeded` uses the player order of
the editor, the best player first.
-}
type Order
    = Shuffled
    | Seeded


{-| Which players of the running tournament take part in the next one.
Removed players never do.
-}
type Entrants
    = StillIn
    | AllButRemoved


type ModeSettings
    = KnockoutSettings Bracket.Knockout.Settings
    | RoundRobinSettings Bracket.RoundRobin.Settings
    | SwissSettings Bracket.Swiss.Settings
    | StepladderSettings Bracket.Stepladder.Settings
    | DoubleEliminationSettings Bracket.DoubleElimination.Settings


{-| Identifies a mode without its settings.
-}
type Mode
    = KnockoutMode
    | RoundRobinMode
    | SwissMode
    | StepladderMode
    | DoubleEliminationMode


type Tournament
    = Knockout Bracket.Knockout.State
    | RoundRobin Bracket.RoundRobin.State
    | Swiss Bracket.Swiss.State
    | Stepladder Bracket.Stepladder.State
    | DoubleElimination Bracket.DoubleElimination.State


type Msg
    = KnockoutMsg Bracket.Knockout.Msg
    | RoundRobinMsg Bracket.RoundRobin.View.Msg
    | SwissMsg Bracket.Swiss.View.Msg
    | StepladderMsg Bracket.Stepladder.Msg
    | DoubleEliminationMsg Bracket.Knockout.Msg


{-| The view state of one viewer. It isn't shared. `highlight` is the player
whose path through the knockout or double elimination bracket is highlighted.
-}
type alias Local =
    { roundRobin : Bracket.RoundRobin.View.Model
    , swiss : Bracket.Swiss.View.Model
    , highlight : Maybe PlayerId
    }


initLocal : Local
initLocal =
    { roundRobin = Bracket.RoundRobin.View.init
    , swiss = Bracket.Swiss.View.init
    , highlight = Nothing
    }


{-| Viewers who can't edit may send these messages too. They only change the
`Local` state.
-}
isViewerMsg : Msg -> Bool
isViewerMsg msg =
    case msg of
        KnockoutMsg (Bracket.Knockout.Highlight _) ->
            True

        DoubleEliminationMsg (Bracket.Knockout.Highlight _) ->
            True

        SwissMsg (Bracket.Swiss.View.ToggleRound _) ->
            True

        _ ->
            False


defaultSettings : Settings
defaultSettings =
    { order = Shuffled
    , entrants = StillIn
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

        StepladderSettings _ ->
            StepladderMode

        DoubleEliminationSettings _ ->
            DoubleEliminationMode


modeDefaults : Mode -> ModeSettings
modeDefaults mode =
    case mode of
        KnockoutMode ->
            KnockoutSettings Bracket.Knockout.defaultSettings

        RoundRobinMode ->
            RoundRobinSettings Bracket.RoundRobin.defaultSettings

        SwissMode ->
            SwissSettings Bracket.Swiss.defaultSettings

        StepladderMode ->
            StepladderSettings Bracket.Stepladder.defaultSettings

        DoubleEliminationMode ->
            DoubleEliminationSettings Bracket.DoubleElimination.defaultSettings


minPlayers : ModeSettings -> Int
minPlayers mode =
    case mode of
        KnockoutSettings _ ->
            2

        RoundRobinSettings _ ->
            2

        SwissSettings _ ->
            2

        StepladderSettings _ ->
            2

        DoubleEliminationSettings _ ->
            3


{-| Starts a tournament with the players in list order. A seeded knockout or
double elimination places them by their seeds. A stepladder puts the first
player at the top.
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

        StepladderSettings stepladder ->
            Stepladder { settings = stepladder, game = Bracket.Stepladder.start players }

        DoubleEliminationSettings doubleElimination ->
            DoubleElimination
                { settings = doubleElimination
                , game =
                    case settings.order of
                        Shuffled ->
                            Bracket.DoubleElimination.start doubleElimination players

                        Seeded ->
                            Bracket.DoubleElimination.startSeeded doubleElimination players
                }


mapKnockoutGame : (Bracket.Knockout.Game -> Bracket.Knockout.Game) -> Bracket.Knockout.State -> Tournament
mapKnockoutGame mapper state =
    Knockout { state | game = mapper state.game }


addPlayer : PlayerId -> Tournament -> Tournament
addPlayer id tournament =
    case tournament of
        Knockout state ->
            mapKnockoutGame (Bracket.Knockout.Placement.addPlayer state.settings id) state

        RoundRobin state ->
            RoundRobin <| Bracket.RoundRobin.addPlayer id state

        Swiss state ->
            Swiss <| Bracket.Swiss.addPlayer id state

        Stepladder state ->
            Stepladder { state | game = Bracket.Stepladder.addPlayer id state.game }

        DoubleElimination state ->
            DoubleElimination { state | game = Bracket.DoubleElimination.addPlayer state.settings id state.game }


{-| Whether new players still join the running tournament.
-}
acceptsPlayers : Tournament -> Bool
acceptsPlayers tournament =
    case tournament of
        DoubleElimination state ->
            Bracket.DoubleElimination.acceptsPlayers state.game

        _ ->
            True


removePlayer : PlayerId -> Tournament -> Tournament
removePlayer id tournament =
    case tournament of
        Knockout state ->
            mapKnockoutGame (Bracket.Knockout.removePlayer id) state

        RoundRobin state ->
            RoundRobin <| Bracket.RoundRobin.removePlayer id state

        Swiss state ->
            Swiss <| Bracket.Swiss.removePlayer id state

        Stepladder state ->
            Stepladder { state | game = Bracket.Stepladder.removePlayer state.settings id state.game }

        DoubleElimination state ->
            DoubleElimination { state | game = Bracket.DoubleElimination.removePlayer id state.game }


{-| Whether the player takes part and is out, removed or eliminated. In a round
robin and a Swiss tournament only removed players are out.
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

        Stepladder state ->
            Bracket.Stepladder.isEliminated state.settings id state.game

        DoubleElimination state ->
            Bracket.DoubleElimination.isEliminated id state.game


{-| Whether the editor removed the player from the running tournament.
-}
isRemoved : PlayerId -> Tournament -> Bool
isRemoved id tournament =
    case tournament of
        Knockout state ->
            Bracket.Knockout.isRemoved id state.game

        RoundRobin state ->
            Bracket.RoundRobin.isRemoved id state.game

        Swiss state ->
            Bracket.Swiss.isRemoved id state.game

        Stepladder state ->
            Bracket.Stepladder.isRemoved id state.game

        DoubleElimination state ->
            Bracket.DoubleElimination.isRemoved id state.game


{-| Whether the player takes part in the next tournament. Without a running
tournament everyone does.
-}
takesPart : Entrants -> Maybe Tournament -> PlayerId -> Bool
takesPart entrants tournament id =
    case ( entrants, tournament ) of
        ( _, Nothing ) ->
            True

        ( StillIn, Just running ) ->
            not <| isEliminated id running

        ( AllButRemoved, Just running ) ->
            not <| isRemoved id running


update : Msg -> Local -> Tournament -> ( Local, Tournament )
update msg local tournament =
    case ( msg, tournament ) of
        ( KnockoutMsg (Bracket.Knockout.Highlight sub), Knockout _ ) ->
            ( { local | highlight = Bracket.Knockout.updateHighlight sub local.highlight }, tournament )

        ( KnockoutMsg sub, Knockout state ) ->
            ( local, mapKnockoutGame (Bracket.Knockout.update state.settings sub) state )

        ( RoundRobinMsg sub, RoundRobin state ) ->
            Bracket.RoundRobin.View.update sub local.roundRobin state
                |> Tuple.mapBoth (\roundRobin -> { local | roundRobin = roundRobin }) RoundRobin

        ( SwissMsg sub, Swiss state ) ->
            Bracket.Swiss.View.update sub local.swiss state
                |> Tuple.mapBoth (\swiss -> { local | swiss = swiss }) Swiss

        ( StepladderMsg sub, Stepladder state ) ->
            ( local, Stepladder { state | game = Bracket.Stepladder.update state.settings sub state.game } )

        ( DoubleEliminationMsg (Bracket.Knockout.Highlight sub), DoubleElimination _ ) ->
            ( { local | highlight = Bracket.Knockout.updateHighlight sub local.highlight }, tournament )

        ( DoubleEliminationMsg sub, DoubleElimination state ) ->
            ( local, DoubleElimination { state | game = Bracket.DoubleElimination.update state.settings sub state.game } )

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

        Stepladder _ ->
            { hideFinished = False, headers = False }

        DoubleElimination _ ->
            { hideFinished = True, headers = False }


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
                    , highlight = local.highlight
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

        Swiss state ->
            Html.map SwissMsg <|
                Bracket.Swiss.View.view
                    { names = config.names
                    , canEdit = config.canEdit
                    }
                    local.swiss
                    state

        Stepladder state ->
            Html.map StepladderMsg <|
                Bracket.Stepladder.View.view
                    { names = config.names
                    , canEdit = config.canEdit
                    }
                    state

        DoubleElimination state ->
            Html.map DoubleEliminationMsg <|
                Bracket.DoubleElimination.View.view
                    { names = config.names
                    , canEdit = config.canEdit
                    , hideFinished = config.box.hide
                    , highlight = local.highlight
                    }
                    state


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

        Stepladder _ ->
            Nothing

        DoubleElimination _ ->
            Nothing


placements : Tournament -> List Placement
placements tournament =
    case tournament of
        Knockout state ->
            Bracket.Knockout.Placement.placements state.settings state.game

        RoundRobin state ->
            Bracket.RoundRobin.placements state

        Swiss state ->
            Bracket.Swiss.placements state

        Stepladder state ->
            Bracket.Stepladder.placements state.settings state.game

        DoubleElimination state ->
            Bracket.DoubleElimination.placements state.game


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
        |> required "order" decodeOrder
        |> optional "entrants" decodeEntrants StillIn
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

                        "swiss" ->
                            JD.field "settings" Bracket.Swiss.Serializer.decodeSettings
                                |> JD.map SwissSettings

                        "stepladder" ->
                            JD.field "settings" Bracket.Stepladder.Serializer.decodeSettings
                                |> JD.map StepladderSettings

                        "doubleElimination" ->
                            JD.field "settings" Bracket.DoubleElimination.Serializer.decodeSettings
                                |> JD.map DoubleEliminationSettings

                        _ ->
                            unknownMode mode
            )


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "order", encodeOrder settings.order )
        , ( "entrants", encodeEntrants settings.entrants )
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

                StepladderSettings stepladder ->
                    JE.object
                        [ ( "mode", JE.string "stepladder" )
                        , ( "settings", Bracket.Stepladder.Serializer.encodeSettings stepladder )
                        ]

                DoubleEliminationSettings doubleElimination ->
                    JE.object
                        [ ( "mode", JE.string "doubleElimination" )
                        , ( "settings", Bracket.DoubleElimination.Serializer.encodeSettings doubleElimination )
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


decodeEntrants : Decoder Entrants
decodeEntrants =
    JD.string
        |> JD.andThen
            (\entrants ->
                case entrants of
                    "stillIn" ->
                        JD.succeed StillIn

                    "allButRemoved" ->
                        JD.succeed AllButRemoved

                    _ ->
                        JD.fail <| "unknown entrants: " ++ entrants
            )


encodeEntrants : Entrants -> Value
encodeEntrants entrants =
    JE.string <|
        case entrants of
            StillIn ->
                "stillIn"

            AllButRemoved ->
                "allButRemoved"


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

                "swiss" ->
                    JD.succeed Bracket.Swiss.State
                        |> required "settings" Bracket.Swiss.Serializer.decodeSettings
                        |> required "game" Bracket.Swiss.Serializer.decodeGame
                        |> JD.map Swiss

                "stepladder" ->
                    JD.succeed Bracket.Stepladder.State
                        |> required "settings" Bracket.Stepladder.Serializer.decodeSettings
                        |> required "game" Bracket.Stepladder.Serializer.decodeGame
                        |> JD.map Stepladder

                "doubleElimination" ->
                    JD.succeed Bracket.DoubleElimination.State
                        |> required "settings" Bracket.DoubleElimination.Serializer.decodeSettings
                        |> required "game" Bracket.DoubleElimination.Serializer.decodeGame
                        |> JD.map DoubleElimination

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

        Stepladder state ->
            JE.object
                [ ( "mode", JE.string "stepladder" )
                , ( "settings", Bracket.Stepladder.Serializer.encodeSettings state.settings )
                , ( "game", Bracket.Stepladder.Serializer.encodeGame state.game )
                ]

        DoubleElimination state ->
            JE.object
                [ ( "mode", JE.string "doubleElimination" )
                , ( "settings", Bracket.DoubleElimination.Serializer.encodeSettings state.settings )
                , ( "game", Bracket.DoubleElimination.Serializer.encodeGame state.game )
                ]
