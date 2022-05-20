module Message exposing (ClimbingRoutesPageMsg(..), Msg(..))

import Data exposing (Ascent, ClimbingRoute, ClimbingRouteKind, Media, Sector)
import DatePicker
import File exposing (File)
import Model
import Select


type Msg
    = Dummy
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested
    | SetModal Model.ModalContent
    | ToggleSettings
      -- Pages
    | SetPage Model.Page
    | ClimbingRoutesPageMessage ClimbingRoutesPageMsg
      -- Data
    | SaveClimbingRouteForm
    | AddMediaToRoute ClimbingRoute
    | RemoveMedia ClimbingRoute Media
    | DeleteClimbingRouteConfirmation ClimbingRoute
    | DeleteClimbingRouteRequested
    | SaveAscentForm
    | DeleteAscentConfirmation Ascent
    | DeleteAscentRequested Ascent


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
