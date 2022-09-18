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


type alias Media =
    { link : String, label : String }


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
