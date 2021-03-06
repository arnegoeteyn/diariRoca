module Message exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind, Media, Sector, Trip)
import DatePicker
import File exposing (File)
import Model exposing (AreaForm, AscentForm, ClimbingRouteForm, SectorForm, TripForm)
import Select


type Msg
    = Dummy
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested
    | AuthorizeGoogleDrive
    | GoogleDriveResponse { type_ : String, argument : Maybe String }
    | GoogleDriveJsonRequested
    | GoogleDriveExportRequested
    | CloseModal
    | ToggleSettings
      -- Global
    | ShowClimbingRoute ClimbingRoute
      -- Pages
    | SetPage Model.Page
    | ClimbingRoutesPageMessage ClimbingRoutesPageMsg
    | SectorsPageMessage SectorsPageMsg
      -- Data - Trip
    | OpenTripForm (Maybe Trip)
    | OpenTripOverview Trip
      -- Data - Area
    | OpenAreaForm (Maybe Area)
    | DeleteAreaRequested Area
    | DeleteAreaConfirmation Area
      -- Data - Sector
    | OpenSectorForm (Maybe Sector)
    | DeleteSectorRequested Sector
    | DeleteSectorConfirmation Sector
      -- Data - ClimbingRoute
    | OpenClimbingRouteForm (Maybe ClimbingRoute)
    | AddMediaToRoute ClimbingRoute
    | RemoveMedia ClimbingRoute Media
    | DeleteClimbingRouteConfirmation ClimbingRoute
    | DeleteClimbingRouteRequested
      -- Data - Ascent
    | OpenAscentForm (Maybe Ascent) ClimbingRoute
    | DeleteAscentConfirmation Ascent
    | DeleteAscentRequested Ascent
      -- Extensions
    | FormMessage FormMsg


type ClimbingRoutesPageMsg
    = -- Filter
      SetRouteFilter String
    | SelectMsg (Select.Msg Sector)
    | SelectSector (Maybe Sector)
    | OnRemoveSectorSelection Sector
    | SetClimbingRouteKindFilter (Maybe ClimbingRouteKind)
      -- Route list
    | OnClimbingRouteClicked (Maybe ClimbingRoute)
      -- Route Detail
    | SetMediaLink String
    | SetMediaLabel String


type SectorsPageMsg
    = AreaSelected (Maybe Area)


type FormMsg
    = -- Trip
      UpdateTripForm TripForm
    | FromTripFormToDatePicker DatePicker.Msg
    | ToTripFormToDatePicker DatePicker.Msg
    | SaveTripForm
      -- Area
    | UpdateAreaForm AreaForm
    | SaveAreaForm
      -- Sector
    | UpdateSectorForm SectorForm
    | SectorFormSelectArea (Maybe Area)
    | SectorFormSelectAreaMsg (Select.Msg Area)
    | SaveSectorForm
      -- ClimbingRoute
    | UpdateClimbingRouteForm ClimbingRouteForm
    | ClimbingRouteFormSelectSector (Maybe Sector)
    | ClimbingRouteFormSelectSectorMsg (Select.Msg Sector)
    | SaveClimbingRouteForm
      -- Ascent
    | UpdateAscentForm AscentForm
    | AscentFormToDatePicker DatePicker.Msg
    | SaveAscentForm
