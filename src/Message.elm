module Message exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind, Media, Sector)
import DatePicker
import File exposing (File)
import Model exposing (AreaForm, AscentForm, ClimbingRouteForm, SectorForm)
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
      -- Pages
    | SetPage Model.Page
    | ClimbingRoutesPageMessage ClimbingRoutesPageMsg
    | SectorsPageMessage SectorsPageMsg
      -- Data
    | OpenAreaForm (Maybe Area)
    | OpenSectorForm (Maybe Area)
    | OpenClimbingRouteForm (Maybe ClimbingRoute)
    | AddMediaToRoute ClimbingRoute
    | RemoveMedia ClimbingRoute Media
    | DeleteClimbingRouteConfirmation ClimbingRoute
    | DeleteClimbingRouteRequested
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
    = UpdateAreaForm AreaForm
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
