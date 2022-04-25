module Message exposing (..)

import Data exposing (ClimbingRoute, Sector)
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
      -- Filter
    | SetRouteFilter String
    | SelectMsg (Select.Msg Sector)
    | SelectSector (Maybe Sector)
    | OnRemoveSectorSelection Sector
      -- Route list
    | OnClimbingRouteClicked (Maybe ClimbingRoute)
    | SetModal Model.ModalContent
    | DeleteClimbingRouteRequested
    | DeleteClimbingRouteConfirmation ClimbingRoute
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
