module Data exposing (..)

import Codec exposing (decodeDict, encodeDict, nullable)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE
import List.Extra
import PlayerId exposing (PlayerId)
import Tournament exposing (Settings, Tournament)


type alias Data =
    { title : String
    , nextId : PlayerId
    , users : Dict PlayerId String
    , order : List PlayerId
    , origin : String
    , nextSettings : Settings
    , tournament : Maybe Tournament
    }


{-| Why data can't be loaded. Data of a newer version needs a newer app.
-}
type LoadError
    = NewerVersion
    | Invalid String


{-| The version of the format that `encodeData` writes. Every change of the
format increments it, and `decodeData` keeps reading all older versions except
the skipped ones.
-}
currentVersion : Int
currentVersion =
    9


{-| Versions of the format that were never released. Data with them is
rejected.
-}
skippedVersions : List Int
skippedVersions =
    [ 1, 2, 3, 4 ]


isSupported : Int -> Bool
isSupported version =
    version >= 1 && version <= currentVersion && not (List.member version skippedVersions)


init : Data
init =
    { title = "Tournament"
    , nextId = 0
    , users = Dict.empty
    , order = []
    , origin = ""
    , nextSettings = Tournament.defaultSettings
    , tournament = Nothing
    }


decodeVersion : Decoder Int
decodeVersion =
    JD.field "version" JD.int


{-| Fails for data without a version, of a newer version and of a skipped
version.
-}
decodeData : Decoder Data
decodeData =
    decodeVersion
        |> JD.andThen
            (\version ->
                if version > currentVersion then
                    JD.fail <|
                        "the data has the version "
                            ++ String.fromInt version
                            ++ ", but this app only reads up to version "
                            ++ String.fromInt currentVersion

                else if not <| isSupported version then
                    JD.fail <| "unsupported version " ++ String.fromInt version

                else
                    decodeContent
            )


decodeContent : Decoder Data
decodeContent =
    JD.succeed Data
        |> required "title" JD.string
        |> required "nextId" PlayerId.decode
        |> required "users" (decodeDict PlayerId.decode JD.string)
        |> required "order" (JD.list PlayerId.decode)
        |> required "origin" JD.string
        |> required "nextSettings" Tournament.decodeSettings
        |> required "tournament" (JD.nullable Tournament.decodeTournament)
        |> JD.map (\data -> { data | order = fixOrder data.users data.order })


{-| The order has every player exactly once. Players missing in it are added
in the order they were added to the tournament.
-}
fixOrder : Dict PlayerId String -> List PlayerId -> List PlayerId
fixOrder users order =
    let
        known =
            List.Extra.unique <| List.filter (\id -> Dict.member id users) order
    in
    known ++ List.filter (\id -> not <| List.member id known) (Dict.keys users)


{-| Like `decodeData`, but tells data of a newer version apart from broken
data.
-}
load : Value -> Result LoadError Data
load value =
    case JD.decodeValue decodeData value of
        Ok data ->
            Ok data

        Err error ->
            case JD.decodeValue decodeVersion value of
                Ok version ->
                    if version > currentVersion then
                        Err NewerVersion

                    else
                        Err <| Invalid <| JD.errorToString error

                Err _ ->
                    Err <| Invalid <| JD.errorToString error


encodeData : Data -> Value
encodeData data =
    JE.object
        [ ( "version", JE.int currentVersion )
        , ( "title", JE.string data.title )
        , ( "nextId", PlayerId.encode data.nextId )
        , ( "users", encodeDict PlayerId.encode JE.string data.users )
        , ( "order", JE.list PlayerId.encode data.order )
        , ( "origin", JE.string data.origin )
        , ( "nextSettings", Tournament.encodeSettings data.nextSettings )
        , ( "tournament", nullable Tournament.encodeTournament data.tournament )
        ]


{-| New players are appended to the order.
-}
addPlayer : String -> Data -> Data
addPlayer name data =
    { data
        | users = Dict.insert data.nextId name data.users
        , order = data.order ++ [ data.nextId ]
        , nextId = data.nextId + 1
        , tournament = Maybe.map (Tournament.addPlayer data.nextId) data.tournament
    }


{-| Only keeps the players for which the check is true.
-}
filterPlayers : (PlayerId -> Bool) -> Data -> Data
filterPlayers keep data =
    { data
        | users = Dict.filter (\id _ -> keep id) data.users
        , order = List.filter keep data.order
    }


{-| Swaps the player with the next player in the order that `movable` accepts:
upwards for a `step` of -1, downwards for 1.
-}
movePlayer : (PlayerId -> Bool) -> PlayerId -> Int -> Data -> Data
movePlayer movable id step data =
    let
        candidates =
            List.filter movable data.order
    in
    case
        List.Extra.elemIndex id candidates
            |> Maybe.andThen (\index -> List.Extra.getAt (index + step) candidates)
    of
        Just other ->
            { data
                | order =
                    List.map
                        (\x ->
                            if x == id then
                                other

                            else if x == other then
                                id

                            else
                                x
                        )
                        data.order
            }

        Nothing ->
            data
