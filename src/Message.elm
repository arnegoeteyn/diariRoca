module Message exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind, Media, Sector)
import DatePicker
import File exposing (File)
import Form.View
import Forms.Form exposing (Form)
import Model exposing (AreaForm, AreaFormValues, ClimbingRouteFormValues, SectorForm, SectorFormValues, ValidatedSectorFormValuesConstructor)
import Select


type Msg
    = Dummy
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested
    | SetModal Model.ModalContent -- TODO: Should not be a message
    | ToggleSettings
      -- Pages
    | SetPage Model.Page
    | ClimbingRoutesPageMessage ClimbingRoutesPageMsg
      -- Data
    | OpenAreaForm (Maybe Area)
    | OpenSectorForm (Maybe Area)
    | SaveClimbingRouteForm
    | AddMediaToRoute ClimbingRoute
    | RemoveMedia ClimbingRoute Media
    | DeleteClimbingRouteConfirmation ClimbingRoute
    | DeleteClimbingRouteRequested
    | SaveAscentForm
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
    | SetMediaLink (Maybe String)
    | SetMediaLabel (Maybe String)
      --| ClimbingRoute Form
    | UpdateClimbingRouteForm Model.ClimbingRouteForm
    | FormSelectSector (Maybe Sector)
    | FormSelectSectorMsg (Select.Msg Sector)
    | OnFormRemoveSectorSelection Sector
      --| Ascent Form
    | UpdateAscentForm Model.AscentForm
    | ToDatePickerAscentForm DatePicker.Msg



--| Forms


type FormMsg
    = UpdateAreaForm AreaForm
    | SaveAreaForm
      -- Sector
    | UpdateSectorForm SectorForm
    | SectorFormSelectArea (Maybe Area)
    | SectorFormSelectAreaMsg (Select.Msg Area)
    | SaveSectorForm
      -- ClimbingRoute
    | NewClimbingRoute ClimbingRoute
    | ClimbingRouteValues (Form.View.Model ClimbingRouteFormValues)
