module Model exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Sector, Trip)
import Date exposing (Date)
import Dict exposing (Dict)
import Select
import Time


type alias Model =
    { appState : AppState
    , startUpDate : Date

    -- Data
    , climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , areas : Dict Int Area
    , trips : Dict Int Trip

    -- UI
    , routeFilter : String
    , selected : List Sector
    , selectState : Select.State
    , selectedClimbingRoute : Maybe ClimbingRoute
    , mediaInput : String
    , modal : ModalContent

    -- Forms
    , climbingRouteForm : ClimbingRouteForm
    , ascentForm : AscentForm
    }


type AppState
    = NotReady
    | Ready


type alias ClimbingRouteForm =
    { name : Maybe String
    , grade : Maybe String
    , comment : Maybe String
    , sectorId : Maybe Int
    , id : Maybe Int
    , selected : List Sector
    , selectState : Select.State
    }


type alias AscentForm =
    {}


type ModalContent
    = Empty
    | ClimbingRouteFormModal
    | AscentFormModal
    | DeleteClimbingRouteRequestModal
