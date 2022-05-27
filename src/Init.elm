module Init exposing (..)

import Data exposing (Area, Sector, jsonFileDecoder)
import DataUtilities
import Date exposing (Date)
import DatePicker exposing (defaultSettings)
import Dict
import Form.View
import Forms.Form exposing (Form(..))
import Json.Decode exposing (decodeString)
import Message exposing (ClimbingRoutesPageMsg(..), FormMsg(..), Msg(..))
import Model exposing (AreaForm, AreaFormValues, AscentForm, ClimbingRouteForm, ClimbingRoutesPageModel, Model, Page(..), SectorForm, SectorFormValues, ValidatedSectorFormValuesConstructor)
import Select
import Time
import Utilities


init : { storageCache : String, posixTime : Int } -> ( Model, Cmd Msg )
init { storageCache, posixTime } =
    let
        date =
            Date.fromPosix Time.utc (Time.millisToPosix posixTime)

        decodedStorage =
            decodeString jsonFileDecoder storageCache

        jsonFile =
            Result.withDefault { climbingRoutes = Dict.empty, ascents = Dict.empty, sectors = Dict.empty, areas = Dict.empty, trips = Dict.empty } <| decodedStorage

        ( climbingRoutesPageModel, climbingRoutesPageCmd ) =
            initClimbingRoutesPage date
    in
    ( { appState =
            Model.Ready
      , startUpDate = Date.fromPosix Time.utc (Time.millisToPosix posixTime)
      , page = ClimbingRoutesPage
      , modal = Model.Empty
      , settingsOpen = False

      -- Data
      , climbingRoutes = jsonFile.climbingRoutes
      , ascents = jsonFile.ascents
      , sectors = jsonFile.sectors
      , areas = jsonFile.areas
      , trips = jsonFile.trips

      -- Forms
      , areaForm = initAreaForm
      , sectorForm = initSectorForm
      , climbingRouteForm = Idle { name = "", grade = "" }
      , areaFormId = -1
      , sectorFormId = -1

      -- Pages
      , climbingRoutesPageModel = climbingRoutesPageModel
      }
    , climbingRoutesPageCmd
    )


initAreaForm : AreaForm
initAreaForm =
    Idle
        { name = ""
        , country = ""
        }


initSectorForm : SectorForm
initSectorForm =
    Idle
        { name = ""
        , areaId = ( [], Select.init "sectorFormAreaId" )
        }


sectorFormAreaSelectConfig : Select.Config Msg Area
sectorFormAreaSelectConfig =
    let
        r : Select.RequiredConfig Message.Msg Area
        r =
            { filter = \x y -> DataUtilities.filterAreasByName x y |> Utilities.listToMaybe
            , toLabel = .name
            , onSelect = FormMessage << SectorFormSelectArea
            , toMsg = FormMessage << SectorFormSelectAreaMsg
            }
    in
    Select.newConfig r



-- |> Select.withOnRemoveItem (wrapCrpMessage OnFormRemoveSectorSelection)
--| Pages


initClimbingRoutesPage : Date -> ( ClimbingRoutesPageModel, Cmd Msg )
initClimbingRoutesPage date =
    let
        ( ascentForm, ascentFormCmd ) =
            initAscentForm (Just date)
    in
    ( { climbingRouteForm = initClimbingRouteForm
      , ascentForm = ascentForm
      , routeFilter = ""
      , routeKindFilter = Nothing
      , selected = []
      , selectState = Select.init "sectors"
      , selectedClimbingRoute = Nothing
      , mediaLink = Nothing
      , mediaLabel = Nothing
      }
    , ascentFormCmd
    )



--| Forms


initClimbingRouteForm : ClimbingRouteForm
initClimbingRouteForm =
    { name = Nothing
    , grade = Nothing
    , sectorId = Nothing
    , comment = Nothing
    , kind = Nothing
    , id = Nothing
    , selected = []
    , selectState = Select.init "formSector"
    }


initAscentForm : Maybe Date -> ( AscentForm, Cmd Msg )
initAscentForm mDate =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { comment = Nothing
      , date = mDate
      , kind = Nothing
      , id = Nothing
      , datePicker = datePicker
      }
    , Cmd.map (ClimbingRoutesPageMessage << ToDatePickerAscentForm) datePickerFx
    )


ascentFormDatePickerSettings : DatePicker.Settings
ascentFormDatePickerSettings =
    defaultSettings



--| Selections


sectorSelectConfig : Select.Config Msg Sector
sectorSelectConfig =
    let
        r : Select.RequiredConfig Msg Sector
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = .name
            , onSelect = ClimbingRoutesPageMessage << SelectSector
            , toMsg = wrapCrpMessage SelectMsg
            }
    in
    Select.newConfig r
        |> Select.withMultiSelection True
        |> Select.withOnRemoveItem (wrapCrpMessage OnRemoveSectorSelection)


formSectorSelectConfig : Select.Config Msg Sector
formSectorSelectConfig =
    let
        r : Select.RequiredConfig Message.Msg Sector
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = .name
            , onSelect = wrapCrpMessage FormSelectSector
            , toMsg = wrapCrpMessage FormSelectSectorMsg
            }
    in
    Select.newConfig r
        |> Select.withOnRemoveItem (wrapCrpMessage OnFormRemoveSectorSelection)



--| Utilities


wrapCrpMessage : (a -> ClimbingRoutesPageMsg) -> a -> Msg
wrapCrpMessage msg =
    ClimbingRoutesPageMessage << msg
