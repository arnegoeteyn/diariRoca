module Model exposing (..)

import Data exposing (Area, Ascent, AscentKind, ClimbingRoute, ClimbingRouteKind, Sector, Trip)
import Date exposing (Date)
import DatePicker exposing (DatePicker)
import Dict exposing (Dict)
import Forms.Form exposing (Form)
import Select


type alias Model =
    { appState : AppState
    , startUpDate : Date
    , page : Page
    , modal : ModalContent
    , settingsOpen : Bool

    -- Data
    , climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , areas : Dict Int Area
    , trips : Dict Int Trip

    -- Forms
    , areaForm : AreaForm
    , areaFormId : Int
    , sectorForm : SectorForm
    , sectorFormId : Int
    , climbingRouteForm : ClimbingRouteForm
    , climbingRouteFormId : Int

    -- Pages
    , climbingRoutesPageModel : ClimbingRoutesPageModel
    }


type alias ClimbingRoutesPageModel =
    { selectedClimbingRoute : Maybe ClimbingRoute

    -- Filters
    , routeFilter : String
    , routeKindFilter : Maybe ClimbingRouteKind
    , selected : List Sector
    , selectState : Select.State

    -- Forms
    -- , climbingRouteForm : ClimbingRouteForm
    , ascentForm : AscentForm
    , mediaLink : Maybe String
    , mediaLabel : Maybe String
    }


type AppState
    = NotReady
    | Ready


type alias AscentForm =
    { comment : Maybe String
    , kind : Maybe AscentKind
    , date : Maybe Date
    , id : Maybe Int
    , datePicker : DatePicker
    }


type ModalContent
    = Empty
    | AreaFormModal
    | SectorFormModal
    | ClimbingRouteFormModal
    | AscentFormModal
    | DeleteClimbingRouteRequestModal
    | DeleteAscentRequestModal Ascent


type Page
    = ClimbingRoutesPage
    | AscentsPage
    | StatsPage



--| Forms


type alias SelectionCriterium item =
    ( List item, Select.State )


type alias AreaFormValues =
    { name : String
    , country : String
    }


type alias AreaForm =
    Form AreaFormValues AreaFormValues


type alias SectorFormValues =
    { name : String
    , areaId : SelectionCriterium Area
    }


type alias ValidatedSectorFormValues =
    { name : String
    , areaId : Int
    }


type alias ValidatedSectorFormValuesConstructor =
    ValidatedSectorFormValues


type alias SectorForm =
    Form SectorFormValues ValidatedSectorFormValuesConstructor


type alias ValidatedSectorForm =
    Form SectorFormValues ValidatedSectorFormValues


type alias ClimbingRouteFormValues =
    { name : String
    , grade : String
    , comment : String
    , sectorId : SelectionCriterium Sector
    , kind : String
    }


type alias ValidatedClimbingRouteFormValues =
    { name : String
    , grade : String
    , comment : Maybe String
    , kind : ClimbingRouteKind
    , sectorId : Int
    }


type alias ClimbingRouteForm =
    Form ClimbingRouteFormValues ValidatedClimbingRouteFormValues


type alias ValidatedClimbingRouteFormValuesConstructor =
    ValidatedClimbingRouteFormValues


type alias ValidatedClimbingRouteForm =
    Form ClimbingRouteFormValues ValidatedClimbingRouteFormValues
