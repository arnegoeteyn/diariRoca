module Data exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)


type alias Data =
    { climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , areas : Dict Int Area
    , trips : Dict Int Trip
    }


type alias ClimbingRoute =
    { id : Int
    , sectorId : Int
    , name : String
    , grade : String
    , comment : Maybe String
    , beta : Maybe String
    , kind : ClimbingRouteKind
    , media : List Media
    }


type ClimbingRouteKind
    = Boulder
    | Sport


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


climbingRouteKindEnum : List ClimbingRouteKind
climbingRouteKindEnum =
    [ Sport, Boulder ]


type alias Media =
    { link : String, label : String }



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


ascentKindEnum : List AscentKind
ascentKindEnum =
    [ Onsight, Flash, SecondGo, Redpoint, Repeat ]



--| Sector


type alias Sector =
    { id : Int
    , areaId : Int
    , name : String
    }


type alias Area =
    { id : Int
    , name : String
    , country : String
    }


type alias Trip =
    { id : Int
    , from : Date
    , to : Date
    }
