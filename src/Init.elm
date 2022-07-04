module Init exposing (..)

import Data exposing (Area, Ascent, AscentKind(..), ClimbingRoute, ClimbingRouteKind(..), Sector, Trip, ascentKindToString, climbingRouteKindToString, jsonFileDecoder)
import DataUtilities
import Date exposing (Date)
import DatePicker exposing (defaultSettings)
import Dict
import Forms.Form exposing (Form(..))
import Json.Decode exposing (decodeString)
import Message exposing (ClimbingRoutesPageMsg(..), FormMsg(..), Msg(..))
import Model exposing (AreaForm, AscentForm, ClimbingRouteForm, ClimbingRoutesPageModel, Model, Page(..), SectorForm, SectorsPageModel, TripForm)
import ModelAccessors as MA
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
            initAscentForm date Nothing

        ( tripForm, tripFormCmd ) =
            initTripForm Nothing date
    in
    ( { appState =
            Model.Ready
      , startUpDate = Date.fromPosix Time.utc (Time.millisToPosix posixTime)
      , page = ClimbingRoutesPage
      , modal = Model.Empty
      , settingsOpen = False
      , googleDriveAuthorized = False

      -- Data
      , climbingRoutes = jsonFile.climbingRoutes
      , ascents = jsonFile.ascents
      , sectors = jsonFile.sectors
      , areas = jsonFile.areas
      , trips = jsonFile.trips

      -- Forms
      , tripForm = ( tripForm, Nothing )
      , areaForm = ( initAreaForm Nothing, Nothing )
      , sectorForm = ( initSectorForm Nothing Nothing, Nothing )
      , climbingRouteForm = ( initClimbingRouteForm Nothing Nothing, Nothing )
      , ascentForm = ( ascentForm, Nothing )

      -- Pages
      , climbingRoutesPageModel = initClimbingRoutesPage
      , sectorsPageModel = initSectorsPage
      }
    , Cmd.batch [ tripFormCmd, ascentFormCmd ]
    )


initTripForm : Maybe Trip -> Date -> ( TripForm, Cmd Msg )
initTripForm maybeTrip defaultDate =
    let
        ( fromDatePicker, fromDatePickerFx ) =
            DatePicker.init

        ( toDatePicker, toDatePickerFx ) =
            DatePicker.init
    in
    ( Idle
        { from = ( Date.toRataDie (Maybe.map .from maybeTrip |> Maybe.withDefault defaultDate), fromDatePicker )
        , to = ( Date.toRataDie (Maybe.map .to maybeTrip |> Maybe.withDefault defaultDate), toDatePicker )
        }
    , Cmd.batch
        [ Cmd.map (FormMessage << FromTripFormToDatePicker) fromDatePickerFx
        , Cmd.map (FormMessage << ToTripFormToDatePicker) toDatePickerFx
        ]
    )


initAreaForm : Maybe Area -> AreaForm
initAreaForm maybeArea =
    Idle
        { name = Maybe.map .name maybeArea |> Maybe.withDefault ""
        , country = Maybe.map .country maybeArea |> Maybe.withDefault ""
        }


initSectorForm : Maybe Model -> Maybe Sector -> SectorForm
initSectorForm maybeModel maybeSector =
    Idle
        { name = Maybe.map .name maybeSector |> Maybe.withDefault ""
        , areaId =
            ( Maybe.andThen
                (\model ->
                    Maybe.andThen (.areaId >> MA.getArea model) maybeSector
                        |> Maybe.map List.singleton
                )
                maybeModel
                |> Maybe.withDefault []
            , Select.init "sectorFormAreaId"
            )
        }


initClimbingRouteForm : Maybe Model -> Maybe ClimbingRoute -> ClimbingRouteForm
initClimbingRouteForm maybeModel climbingRoute =
    Idle
        { name = Maybe.map .name climbingRoute |> Maybe.withDefault ""
        , grade = Maybe.map .grade climbingRoute |> Maybe.withDefault ""
        , comment = Maybe.andThen .comment climbingRoute |> Maybe.withDefault ""
        , kind = climbingRouteKindToString <| ((Maybe.withDefault Sport <| Maybe.map .kind climbingRoute) |> Debug.log "Kind is")
        , sectorId =
            ( Maybe.andThen (\model -> Maybe.andThen (.sectorId >> MA.getSector model) climbingRoute |> Maybe.map List.singleton) maybeModel |> Maybe.withDefault []
            , Select.init "climbingRouteFormSectorId"
            )
        }


initAscentForm : Date -> Maybe Ascent -> ( AscentForm, Cmd Msg )
initAscentForm date maybeAscent =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( Idle
        { comment = Maybe.andThen .comment maybeAscent |> Maybe.withDefault ""
        , kind = ascentKindToString <| Maybe.withDefault Onsight <| Maybe.map .kind maybeAscent
        , date = ( Date.toRataDie (Maybe.map .date maybeAscent |> Maybe.withDefault date), datePicker )
        }
    , Cmd.map (FormMessage << AscentFormToDatePicker) datePickerFx
    )


initClimbingRoutesPage : ClimbingRoutesPageModel
initClimbingRoutesPage =
    { routeFilter = ""
    , routeKindFilter = Nothing
    , selected = []
    , selectState = Select.init "sectors"
    , selectedClimbingRoute = Nothing
    , mediaLink = ""
    , mediaLabel = ""
    }


initSectorsPage : SectorsPageModel
initSectorsPage =
    { selectedArea = Nothing
    }



--| Dates


tripFormDatePickerSettings : DatePicker.Settings
tripFormDatePickerSettings =
    defaultSettings


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
