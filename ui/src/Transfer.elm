module Transfer exposing
    ( Model
    , Msg
    , Outcome(..)
    , exportLeaderboard
    , fileName
    , importButton
    , init
    , leaderboardFileName
    , readFile
    , update
    , view
    , viewButtons
    )

{-| Saves the data to a file and loads it again.
-}

import Data exposing (Data)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, text)
import Html.Attributes as HA exposing (class)
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Standings
import Task
import Time
import Tournament


{-| An imported file waits for a confirmation or shows why it can't be loaded.
-}
type Model
    = Idle
    | Confirming Data
    | Failed Data.LoadError


type Format
    = Json
    | Csv
    | Leaderboard


type Msg
    = Export Format
    | ExportAt Format ( Time.Zone, Time.Posix )
    | Import
    | Selected File
    | Read String
    | Replace Data
    | Close


init : Model
init =
    Idle


exportLeaderboard : Msg
exportLeaderboard =
    Export Leaderboard


{-| What happens with the content of an imported file.
-}
type Outcome
    = Rejected Data.LoadError
    | AskToReplace Data
    | Load Data


{-| A running tournament is only replaced after a confirmation.
-}
readFile : Data -> String -> Outcome
readFile current content =
    case
        JD.decodeString JD.value content
            |> Result.mapError (JD.errorToString >> Data.Invalid)
            |> Result.andThen Data.load
    of
        Err error ->
            Rejected error

        Ok data ->
            if current.tournament == Nothing then
                Load data

            else
                AskToReplace data


{-| `<title> <YYYY-MM-DD HH-MM>.<extension>` in local time. Characters Windows
doesn't allow in file names become `_`.
-}
fileName : String -> Time.Zone -> Time.Posix -> String -> String
fileName extension zone time title =
    baseName zone time title ++ "." ++ extension


{-| `<title> <YYYY-MM-DD HH-MM> leaderboard.csv`, like `fileName`.
-}
leaderboardFileName : Time.Zone -> Time.Posix -> String -> String
leaderboardFileName zone time title =
    baseName zone time title ++ " leaderboard.csv"


baseName : Time.Zone -> Time.Posix -> String -> String
baseName zone time title =
    let
        pad width =
            String.fromInt >> String.padLeft width '0'

        date =
            String.join "-"
                [ pad 4 <| Time.toYear zone time
                , pad 2 <| monthNumber <| Time.toMonth zone time
                , pad 2 <| Time.toDay zone time
                ]

        clock =
            pad 2 (Time.toHour zone time) ++ "-" ++ pad 2 (Time.toMinute zone time)

        safeTitle =
            case String.trim title of
                "" ->
                    "tournament"

                trimmed ->
                    String.map
                        (\char ->
                            if Char.toCode char < 32 || String.contains (String.fromChar char) "<>:\"/\\|?*" then
                                '_'

                            else
                                char
                        )
                        trimmed
    in
    safeTitle ++ " " ++ date ++ " " ++ clock


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


{-| Returns the data that replaces the current one. Only editors can import.
-}
update : { canEdit : Bool } -> Msg -> Data -> Model -> ( Model, Maybe Data, Cmd Msg )
update config msg data model =
    case msg of
        Export format ->
            ( model
            , Nothing
            , Task.perform (ExportAt format) <| Task.map2 Tuple.pair Time.here Time.now
            )

        ExportAt Json ( zone, time ) ->
            ( model
            , Nothing
            , File.Download.string
                (fileName "json" zone time data.title)
                "application/json"
                (JE.encode 4 <| Data.encodeData data)
            )

        ExportAt Csv ( zone, time ) ->
            ( model
            , Nothing
            , case Maybe.andThen Tournament.csv data.tournament of
                Just toCsv ->
                    File.Download.string
                        (fileName "csv" zone time data.title)
                        "text/csv"
                        (toCsv data.users)

                Nothing ->
                    Cmd.none
            )

        ExportAt Leaderboard ( zone, time ) ->
            ( model
            , Nothing
            , case data.tournament of
                Just tournament ->
                    File.Download.string
                        (leaderboardFileName zone time data.title)
                        "text/csv"
                        (Standings.csv data.users <| Tournament.standings tournament)

                Nothing ->
                    Cmd.none
            )

        Import ->
            ( model
            , Nothing
            , if config.canEdit then
                File.Select.file [ "application/json", ".json" ] Selected

              else
                Cmd.none
            )

        Selected file ->
            ( model, Nothing, Task.perform Read <| File.toString file )

        Read content ->
            if config.canEdit then
                case readFile data content of
                    Rejected error ->
                        ( Failed error, Nothing, Cmd.none )

                    AskToReplace imported ->
                        ( Confirming imported, Nothing, Cmd.none )

                    Load imported ->
                        ( Idle, Just imported, Cmd.none )

            else
                ( model, Nothing, Cmd.none )

        Replace imported ->
            ( Idle
            , if config.canEdit then
                Just imported

              else
                Nothing
            , Cmd.none
            )

        Close ->
            ( Idle, Nothing, Cmd.none )


{-| The buttons for the controls of the game box.
-}
viewButtons : { canEdit : Bool, csv : Bool } -> List (Html Msg)
viewButtons config =
    List.concat
        [ [ button [ HE.onClick <| Export Json, HA.title "Export the tournament to a file" ] [ text "⤓" ] ]
        , if config.canEdit then
            [ button [ HE.onClick Import, HA.title "Import a tournament from a file" ] [ text "⤒" ] ]

          else
            []
        , if config.csv then
            [ button [ class "wide", HE.onClick <| Export Csv, HA.title "Export the table as CSV for a spreadsheet program" ] [ text "CSV" ] ]

          else
            []
        ]


importButton : Html Msg
importButton =
    button [ HE.onClick Import ] [ text "⤒ Import a tournament" ]


view : Model -> Html Msg
view model =
    case model of
        Idle ->
            text ""

        Confirming imported ->
            viewDialog "Replace the current tournament with the file?"
                [ ( Replace imported, "Replace" ), ( Close, "Cancel" ) ]

        Failed error ->
            viewDialog
                (case error of
                    Data.NewerVersion ->
                        "This file can't be loaded. It was saved by a newer version of the app. Reload the app to get the newest version."

                    Data.Invalid _ ->
                        "This file can't be loaded."
                )
                [ ( Close, "OK" ) ]


viewDialog : String -> List ( Msg, String ) -> Html Msg
viewDialog message buttons =
    div [ class "problem-overlay" ]
        [ div [ class "problem" ]
            [ div [ class "problem-message" ] [ text message ]
            , buttons
                |> List.map (\( msg, label ) -> button [ HE.onClick msg ] [ text label ])
                |> div [ class "problem-buttons" ]
            ]
        ]
