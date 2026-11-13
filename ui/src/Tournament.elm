module Tournament exposing
    ( ModeSettings(..)
    , Msg(..)
    , Settings
    , Tournament(..)
    , addPlayer
    , decodeSettings
    , decodeTournament
    , defaultSettings
    , encodeSettings
    , encodeTournament
    , isEliminated
    , minPlayers
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
import Dict exposing (Dict)
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


type Tournament
    = Knockout Bracket.Knockout.State


type Msg
    = KnockoutMsg Bracket.Knockout.Msg


defaultSettings : Settings
defaultSettings =
    { shuffle = True
    , mode = KnockoutSettings Bracket.Knockout.defaultSettings
    }


minPlayers : ModeSettings -> Int
minPlayers mode =
    case mode of
        KnockoutSettings _ ->
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


removePlayer : PlayerId -> Tournament -> Tournament
removePlayer id tournament =
    case tournament of
        Knockout state ->
            mapKnockoutGame (Bracket.Knockout.removePlayer id) state


{-| Whether the player takes part and is out.
-}
isEliminated : PlayerId -> Tournament -> Bool
isEliminated id tournament =
    case tournament of
        Knockout state ->
            Bracket.Knockout.isEliminated id state.game


update : Msg -> Tournament -> Tournament
update msg tournament =
    case ( msg, tournament ) of
        ( KnockoutMsg sub, Knockout state ) ->
            mapKnockoutGame (Bracket.Knockout.update sub) state


{-| Only editors can change results.
-}
view : { names : Dict PlayerId String, hideFinished : Bool, canEdit : Bool } -> Tournament -> Html Msg
view config tournament =
    case tournament of
        Knockout state ->
            Html.map KnockoutMsg <|
                Bracket.Knockout.viewGame { names = config.names, canEdit = config.canEdit } <|
                    if config.hideFinished then
                        Bracket.Knockout.hideFinishedMatches state.game

                    else
                        state.game


placements : Tournament -> List Placement
placements tournament =
    case tournament of
        Knockout state ->
            Bracket.Knockout.Placement.placements state.game


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
