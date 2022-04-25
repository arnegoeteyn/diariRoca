module Init exposing (..)

import Data exposing (Sector, jsonFileDecoder)
import DataUtilities
import Dict
import Json.Decode exposing (decodeString)
import Message exposing (Msg)
import Model exposing (ClimbingRouteForm, Model)
import Select
import Utilities


init : String -> ( Model, Cmd Msg )
init storageCache =
    let
        decodedStorage =
            decodeString jsonFileDecoder storageCache

        jsonFile =
            Result.withDefault { climbingRoutes = Dict.empty, ascents = Dict.empty, sectors = Dict.empty, areas = Dict.empty, trips = Dict.empty } <| decodedStorage
    in
    ( { appState =
            case decodedStorage of
                Result.Ok _ ->
                    Model.Ready

                Result.Err _ ->
                    -- appstate can just default to empty dictionaries
                    Model.Ready
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
      , climbingRouteForm = initClimbingRouteForm
      }
    , Cmd.batch [ Cmd.none ]
    )



--| Forms


initClimbingRouteForm : ClimbingRouteForm
initClimbingRouteForm =
    { name = Nothing
    , grade = Nothing
    , sectorId = Nothing
    , comment = Nothing
    , id = Nothing
    , selected = []
    , selectState = Select.init "formSector"
    }



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
