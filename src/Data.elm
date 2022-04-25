module Data exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode exposing (fail, int, string, succeed)
import Json.Decode.Extra exposing (set)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Set exposing (Set)


type alias JsonFile =
    { climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , areas : Dict Int Area
    , trips : Dict Int Trip
    }


jsonFileDecoder : Json.Decode.Decoder JsonFile
jsonFileDecoder =
    let
        generalDecoder specificDecoder =
            Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.field "id" int) specificDecoder)
                |> Json.Decode.map Dict.fromList

        decodedRoutes =
            generalDecoder climbingRouteDecoder

        decodedAscents =
            generalDecoder ascentsDecoder

        decodedSectors =
            generalDecoder sectorDecoder

        decodedAreas =
            generalDecoder areaDecoder

        decodedTrips =
            generalDecoder tripDecoder
    in
    Json.Decode.map5 JsonFile
        (Json.Decode.field "routes" <| decodedRoutes)
        (Json.Decode.field "ascents" <| decodedAscents)
        (Json.Decode.field "sectors" <| decodedSectors)
        (Json.Decode.field "areas" <| decodedAreas)
        (Json.Decode.field "trips" <| decodedTrips)


encodedJsonFile : JsonFile -> Json.Encode.Value
encodedJsonFile root =
    Json.Encode.object
        [ ( "routes", Json.Encode.list encodeClimbingRoute (Dict.values root.climbingRoutes) )
        , ( "ascents", Json.Encode.list encodeAscent (Dict.values root.ascents) )
        , ( "sectors", Json.Encode.list encodeSector (Dict.values root.sectors) )
        , ( "areas", Json.Encode.list encodeArea (Dict.values root.areas) )
        , ( "trips", Json.Encode.list encodeTrip (Dict.values root.trips) )
        ]



--| ClimbingRoute


type ClimbingRouteKind
    = Boulder
    | Sport


climbingRouteKindDecoder : Json.Decode.Decoder ClimbingRouteKind
climbingRouteKindDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case climbingRouteKindFromString str of
                    Just value ->
                        Json.Decode.succeed value

                    Nothing ->
                        Json.Decode.fail "invalid routeKind"
            )


climbingRouteKindFromString : String -> Maybe ClimbingRouteKind
climbingRouteKindFromString s =
    case String.toLower s of
        "sport" ->
            Just Sport

        "boulder" ->
            Just Boulder

        _ ->
            Nothing


climbingRouteKindToString : ClimbingRouteKind -> String
climbingRouteKindToString kind =
    case kind of
        Sport ->
            "sport"

        Boulder ->
            "boulder"


enumClimbingRouteKind : List ClimbingRouteKind
enumClimbingRouteKind =
    [ Sport, Boulder ]


encodeClimbingRouteKind : ClimbingRouteKind -> Json.Encode.Value
encodeClimbingRouteKind =
    Json.Encode.string << climbingRouteKindToString


type alias ClimbingRoute =
    { id : Int
    , sectorId : Int
    , name : String
    , grade : String
    , comment : Maybe String
    , kind : ClimbingRouteKind
    , media : List String
    }


climbingRouteDecoder : Json.Decode.Decoder ClimbingRoute
climbingRouteDecoder =
    Json.Decode.succeed ClimbingRoute
        |> required "id" int
        |> required "sectorId" int
        |> required "name" string
        |> required "grade" string
        |> optional "comment" (Json.Decode.map Just string) Nothing
        |> required "kind" climbingRouteKindDecoder
        |> optional "media" (Json.Decode.list Json.Decode.string) []


encodeClimbingRoute : ClimbingRoute -> Json.Encode.Value
encodeClimbingRoute route =
    Json.Encode.object
        [ ( "id", Json.Encode.int route.id )
        , ( "sectorId", Json.Encode.int route.sectorId )
        , ( "name", Json.Encode.string route.name )
        , ( "grade", Json.Encode.string route.grade )
        , ( "comment", encodeNullable Json.Encode.string route.comment )
        , ( "kind", encodeClimbingRouteKind route.kind )
        , ( "media", Json.Encode.list Json.Encode.string route.media )
        ]



--| Ascent


type alias Ascent =
    { id : Int
    , routeId : Int
    , date : Date
    , comment : Maybe String
    , kind : AscentKind
    }


type AscentKind
    = Onsight
    | Flash
    | SecondGo
    | Redpoint
    | Repeat


enumAscentKind : List AscentKind
enumAscentKind =
    [ Onsight, Flash, SecondGo, Redpoint, Repeat ]


ascentKindDecoder : Json.Decode.Decoder AscentKind
ascentKindDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case ascentKindFromString str of
                    Just value ->
                        Json.Decode.succeed value

                    _ ->
                        Json.Decode.fail "invalid ascentKind"
            )


encodeAscentKind : AscentKind -> Json.Encode.Value
encodeAscentKind =
    Json.Encode.string << ascentKindToString


ascentKindFromString : String -> Maybe AscentKind
ascentKindFromString s =
    case String.toLower s of
        "onsight" ->
            Just Onsight

        "redpoint" ->
            Just Redpoint

        "flash" ->
            Just Flash

        "repeat" ->
            Just Repeat

        "secondgo" ->
            Just SecondGo

        _ ->
            Nothing


ascentKindToString : AscentKind -> String
ascentKindToString kind =
    case kind of
        Redpoint ->
            "redpoint"

        Flash ->
            "flash"

        Onsight ->
            "onsight"

        SecondGo ->
            "secondgo"

        Repeat ->
            "repeat"


ascentsDecoder : Json.Decode.Decoder Ascent
ascentsDecoder =
    Json.Decode.succeed Ascent
        |> required "id" int
        |> required "routeId" int
        |> required "date" dateDecoder
        |> optional "comment" (Json.Decode.map Just string) Nothing
        |> required "kind" ascentKindDecoder


encodeAscent : Ascent -> Json.Encode.Value
encodeAscent ascent =
    Json.Encode.object
        [ ( "id", Json.Encode.int ascent.id )
        , ( "routeId", Json.Encode.int ascent.routeId )
        , ( "comment", encodeNullable Json.Encode.string ascent.comment )
        , ( "date", Json.Encode.string <| Date.toIsoString ascent.date )
        , ( "kind", encodeAscentKind ascent.kind )
        ]



--| Sector


type alias Sector =
    { id : Int
    , areaId : Int
    , name : String
    }


sectorDecoder : Json.Decode.Decoder Sector
sectorDecoder =
    Json.Decode.succeed Sector
        |> required "id" int
        |> required "areaId" int
        |> required "name" string


encodeSector : Sector -> Json.Encode.Value
encodeSector sector =
    Json.Encode.object
        [ ( "id", Json.Encode.int sector.id )
        , ( "name", Json.Encode.string sector.name )
        , ( "areaId", Json.Encode.int sector.areaId )
        ]



--| Area


type alias Area =
    { id : Int
    , name : String
    , country : String
    }


areaDecoder : Json.Decode.Decoder Area
areaDecoder =
    Json.Decode.succeed Area
        |> required "id" int
        |> required "name" string
        |> required "country" string


encodeArea : Area -> Json.Encode.Value
encodeArea area =
    Json.Encode.object
        [ ( "id", Json.Encode.int area.id )
        , ( "name", Json.Encode.string area.name )
        , ( "country", Json.Encode.string area.country )
        ]



--| Trip


type alias Trip =
    { id : Int
    , from : Date
    , to : Date
    }


tripDecoder : Json.Decode.Decoder Trip
tripDecoder =
    Json.Decode.succeed Trip
        |> required "id" int
        |> required "from" dateDecoder
        |> required "to" dateDecoder


encodeTrip : Trip -> Json.Encode.Value
encodeTrip trip =
    Json.Encode.object
        [ ( "id", Json.Encode.int trip.id )
        , ( "from", Json.Encode.string (Date.toIsoString trip.from) )
        , ( "to", Json.Encode.string (Date.toIsoString trip.to) )
        ]



--| Other


dateDecoder : Json.Decode.Decoder Date
dateDecoder =
    string
        |> Json.Decode.andThen
            (\val ->
                case
                    Date.fromIsoString val
                of
                    Err x ->
                        fail x

                    Ok value ->
                        succeed value
            )



--| Utilities


encodeNullable : (value -> Json.Encode.Value) -> Maybe value -> Json.Encode.Value
encodeNullable valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value

        Nothing ->
            Json.Encode.null
