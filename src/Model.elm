module Model exposing (..)

import Data exposing (Area, Ascent, AscentKind, ClimbingRoute, ClimbingRouteKind, Sector, Trip)
import Date exposing (Date)
import DatePicker exposing (DatePicker)
import Dict exposing (Dict)
import Select


type alias Model =
    { appState : AppState
    , startUpDate : Date
    , page : Page

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
    , kind : Maybe ClimbingRouteKind
    , id : Maybe Int
    , selected : List Sector
    , selectState : Select.State
    }


type alias AscentForm =
    { comment : Maybe String
    , kind : Maybe AscentKind
    , date : Maybe Date
    , id : Maybe Int
    , datePicker : DatePicker
    }


type ModalContent
    = Empty
    | ClimbingRouteFormModal
    | AscentFormModal
    | DeleteClimbingRouteRequestModal
    | DeleteAscentRequestModal Ascent


type Page
    = ClimbingRoutesPage
    | AscentsPage
    | StatsPage
