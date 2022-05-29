module Init exposing (..)

import Data exposing (Area, AscentKind(..), ClimbingRouteKind(..), Sector, ascentKindToString, climbingRouteKindToString, jsonFileDecoder)
import DataUtilities
import Date exposing (Date)
import DatePicker exposing (defaultSettings)
import Dict
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

        ( ascentForm, ascentFormCmd ) =
            initAscentForm date
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
      , areaFormId = -1
      , sectorFormId = -1
      , sectorForm = initSectorForm
      , climbingRouteForm = initClimbingRouteForm
      , climbingRouteFormId = -1
      , ascentForm = ascentForm
      , ascentFormId = -1
      , ascentFormRouteId = -1

      -- Pages
      , climbingRoutesPageModel = climbingRoutesPageModel
      }
    , Cmd.batch [ climbingRoutesPageCmd, ascentFormCmd ]
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


initClimbingRouteForm : ClimbingRouteForm
initClimbingRouteForm =
    Idle
        { name = ""
        , grade = ""
        , comment = ""
        , kind = climbingRouteKindToString Sport
        , sectorId = ( [], Select.init "climbingRouteFormSectorId" )
        }


initAscentForm : Date -> ( AscentForm, Cmd Msg )
initAscentForm date =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( Idle
        { comment = ""
        , kind = ascentKindToString Onsight
        , date = ( Date.toRataDie date, datePicker )
        }
    , Cmd.map (FormMessage << AscentFormToDatePicker) datePickerFx
    )


initClimbingRoutesPage : Date -> ( ClimbingRoutesPageModel, Cmd Msg )
initClimbingRoutesPage date =
    ( { routeFilter = ""
      , routeKindFilter = Nothing
      , selected = []
      , selectState = Select.init "sectors"
      , selectedClimbingRoute = Nothing
      , mediaLink = Nothing
      , mediaLabel = Nothing
      }
    , Cmd.none
    )



--| Dates


ascentFormDatePickerSettings : DatePicker.Settings
ascentFormDatePickerSettings =
    defaultSettings



--| Selections


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


climbingRouteFormSectorSelectConfig : Select.Config Msg Sector
climbingRouteFormSectorSelectConfig =
    let
        r : Select.RequiredConfig Message.Msg Sector
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = .name
            , onSelect = FormMessage << ClimbingRouteFormSelectSector
            , toMsg = FormMessage << ClimbingRouteFormSelectSectorMsg
            }
    in
    Select.newConfig r


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


wrapCrpMessage : (a -> ClimbingRoutesPageMsg) -> a -> Msg
wrapCrpMessage msg =
    ClimbingRoutesPageMessage << msg
