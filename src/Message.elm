module Message exposing (..)

import Data exposing (Ascent, ClimbingRoute, ClimbingRouteKind, Sector)
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
      -- Pages
    | SetPage Model.Page
    | ClimbingRoutesPageMessage ClimbingRoutesPageMsg
      -- Data
    | SaveClimbingRouteForm
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
      --| ClimbingRoute Form
    | UpdateClimbingRouteForm Model.ClimbingRouteForm
    | FormSelectSector (Maybe Sector)
    | FormSelectSectorMsg (Select.Msg Sector)
    | OnFormRemoveSectorSelection Sector
      --| Ascent Form
    | UpdateAscentForm Model.AscentForm
    | ToDatePickerAscentForm DatePicker.Msg
