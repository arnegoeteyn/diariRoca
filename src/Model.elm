module Model exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Sector, Trip)
import Dict exposing (Dict)
import Select


type alias Model =
    { appState : AppState

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


type ModalContent
    = Empty
    | ClimbingRouteFormModal
    | AscentFormModal
    | DeleteClimbingRouteRequestModal
