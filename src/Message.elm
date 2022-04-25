module Message exposing (..)

import Data exposing (ClimbingRoute, Sector)
import File exposing (File)
import Model
import Select


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
      --| Forms
    | UpdateClimbingRouteForm Model.ClimbingRouteForm
    | SaveClimbingRouteForm
    | FormSelectSector (Maybe Sector)
    | FormSelectSectorMsg (Select.Msg Sector)
    | OnFormRemoveSectorSelection Sector
