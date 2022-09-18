module Model exposing (..)

import Browser.Navigation as Nav
import Data exposing (Area, Ascent, AscentKind, ClimbingRoute, ClimbingRouteKind, Sector, Trip)
import Date exposing (Date)
import DatePicker exposing (DatePicker)
import Dict exposing (Dict)
import Forms.Form exposing (Form)
import Select
import Url


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , appState : AppState
    , startUpDate : Date
    , version : String
    , modal : ModalContent
    , settingsOpen : Bool
    , googleDriveAuthorized : Bool

    -- Data
    , climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , areas : Dict Int Area
    , trips : Dict Int Trip

    -- Forms
    , tripForm : ( TripForm, Maybe Trip )
    , areaForm : ( AreaForm, Maybe Area )
    , sectorForm : ( SectorForm, Maybe Sector )
    , climbingRouteForm : ( ClimbingRouteForm, Maybe ClimbingRoute )
    , ascentForm : ( AscentForm, Maybe AscentFormMeta )

    -- Pages
    , climbingRoutesPageModel : ClimbingRoutesPageModel
    , climbingRoutePageModel : ClimbingRoutePageModel
    , sectorsPageModel : SectorsPageModel
    }


type alias ClimbingRoutesPageModel =
    { selectedClimbingRoute : Maybe ClimbingRoute

    -- Filters
    , routeFilter : String
    , routeKindFilter : Maybe ClimbingRouteKind
    , selected : List Sector
    , selectState : Select.State
    }


type alias ClimbingRoutePageModel =
    { mediaLink : String
    , mediaLabel : String
    }


type alias SectorsPageModel =
    { selectedArea : Maybe Area
    }


type AppState
    = NotReady
    | Ready


type ModalContent
    = Empty
    | TripFormModal
    | TripOverviewModal Trip
    | AreaFormModal
    | SectorFormModal
    | ClimbingRouteFormModal
    | AscentFormModal
    | DeleteAreaRequestModal Area
    | DeleteSectorRequestModal Sector
    | DeleteClimbingRouteRequestModal ClimbingRoute
    | DeleteAscentRequestModal Ascent



--| Forms


type alias SelectionCriterium item =
    ( List item, Select.State )


type alias DateCriterium =
    ( Int, DatePicker )


type alias TripFormValues =
    { from : DateCriterium
    , to : DateCriterium
    }


type alias ValidatedTripFormValues =
    { from : Date
    , to : Date
    , id : Int
    }


type alias TripForm =
    Form TripFormValues ValidatedTripFormValues


type alias AreaFormValues =
    { name : String
    , country : String
    }


type alias ValidatedAreaFormValues =
    { name : String
    , country : String
    , id : Int
    }


type alias AreaForm =
    Form AreaFormValues ValidatedAreaFormValues


type alias SectorFormValues =
    { name : String
    , areaId : SelectionCriterium Area
    }


type alias ValidatedSectorFormValues =
    { name : String
    , areaId : Int
    , id : Int
    }


type alias SectorForm =
    Form SectorFormValues ValidatedSectorFormValues


type alias ValidatedSectorForm =
    Form SectorFormValues ValidatedSectorFormValues


type alias ClimbingRouteFormValues =
    { name : String
    , grade : String
    , comment : String
    , beta : String
    , sectorId : SelectionCriterium Sector
    , kind : String
    }


type alias ValidatedClimbingRouteFormValues =
    { id : Int
    , name : String
    , grade : String
    , comment : Maybe String
    , beta : Maybe String
    , kind : ClimbingRouteKind
    , sectorId : Int
    }


type alias ClimbingRouteForm =
    Form ClimbingRouteFormValues ValidatedClimbingRouteFormValues


type alias ValidatedClimbingRouteForm =
    Form ClimbingRouteFormValues ValidatedClimbingRouteFormValues


type alias AscentFormMeta =
    ( Maybe Ascent, ClimbingRoute )


type alias AscentFormValues =
    { date : DateCriterium
    , kind : String
    , comment : String
    }


type alias ValidatedAscentFormValues =
    { id : Int
    , climbingRouteId : Int
    , date : Date
    , kind : AscentKind
    , comment : Maybe String
    }


type alias AscentForm =
    Form AscentFormValues ValidatedAscentFormValues



-- Routing


type Route
    = NotFoundRoute
    | SectorsRoute
    | ClimbingRoutesRoute
    | ClimbingRouteRoute Int
    | AscentsRoute
    | StatsRoute
