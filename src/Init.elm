module Init exposing (..)

import Data exposing (ClimbingRouteKind(..), Sector, jsonFileDecoder)
import DataUtilities
import Date exposing (Date)
import DatePicker exposing (DatePicker, defaultSettings)
import Dict
import Json.Decode exposing (decodeString)
import Message exposing (Msg(..))
import Model exposing (AscentForm, ClimbingRouteForm, Model)
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

        ( ascentForm, ascentFormCmd ) =
            initAscentForm (Just date)
    in
    ( { appState =
            case decodedStorage of
                Result.Ok _ ->
                    Model.Ready

                Result.Err _ ->
                    -- appstate can just default to empty dictionaries
                    Model.Ready
      , startUpDate = Date.fromPosix Time.utc (Time.millisToPosix posixTime)
      , climbingRoutes = jsonFile.climbingRoutes
      , ascents = jsonFile.ascents
      , sectors = jsonFile.sectors
      , areas = jsonFile.areas
      , trips = jsonFile.trips

      -- UI
      , routeFilter = ""
      , selected = []
      , selectState = Select.init "sectors"
      , selectedClimbingRoute = Nothing
      , mediaInput = ""
      , modal = Model.Empty

      --| Forms
      , climbingRouteForm = initClimbingRouteForm
      , ascentForm = ascentForm
      }
    , Cmd.batch [ ascentFormCmd ]
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
    , Cmd.map ToDatePickerAscentForm datePickerFx
    )


ascentFormDatePickerSettings : DatePicker.Settings
ascentFormDatePickerSettings =
    defaultSettings



--| Selections


sectorSelectConfig : Select.Config Message.Msg Sector
sectorSelectConfig =
    let
        r : Select.RequiredConfig Message.Msg Sector
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = .name
            , onSelect = Message.SelectSector
            , toMsg = Message.SelectMsg
            }
    in
    Select.newConfig r
        |> Select.withMultiSelection True
        |> Select.withOnRemoveItem Message.OnRemoveSectorSelection


formSectorSelectConfig : Select.Config Message.Msg Sector
formSectorSelectConfig =
    let
        r : Select.RequiredConfig Message.Msg Sector
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = .name
            , onSelect = Message.FormSelectSector
            , toMsg = Message.FormSelectSectorMsg
            }
    in
    Select.newConfig r
        |> Select.withOnRemoveItem Message.OnFormRemoveSectorSelection
