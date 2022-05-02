module Message exposing (..)

import Data exposing (Ascent, ClimbingRoute, Sector)
import DatePicker exposing (DatePicker)
import File exposing (File)
import Model
import Select
import Time


type Msg
    = Dummy
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested
    | SetPage Model.Page
    | SetModal Model.ModalContent
      -- Filter
    | SetRouteFilter String
    | SelectMsg (Select.Msg Sector)
    | SelectSector (Maybe Sector)
    | OnRemoveSectorSelection Sector
      -- Route list
    | OnClimbingRouteClicked (Maybe ClimbingRoute)
    | DeleteClimbingRouteRequested
    | DeleteClimbingRouteConfirmation ClimbingRoute
      -- Ascent List
    | DeleteAscentRequested Ascent
    | DeleteAscentConfirmation Ascent
      --| ClimbingRoute Form
    | UpdateClimbingRouteForm Model.ClimbingRouteForm
    | SaveClimbingRouteForm
    | FormSelectSector (Maybe Sector)
    | FormSelectSectorMsg (Select.Msg Sector)
    | OnFormRemoveSectorSelection Sector
      --| Ascent Form
    | UpdateAscentForm Model.AscentForm
    | SaveAscentForm
    | ToDatePickerAscentForm DatePicker.Msg
