module General exposing (..)

import Data exposing (ClimbingRoute, Data)
import DataParser exposing (jsonFileDecoder)
import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode exposing (decodeString)



-- Model


type alias Model =
    { data : Data
    , modal : ModalContent
    }


type ModalContent
    = Empty
      -- | TripFormModal
      -- | TripOverviewModal Trip
      -- | AreaFormModal
      -- | SectorFormModal
      -- | ClimbingRouteFormModal
      -- | AscentFormModal
      -- | DeleteAreaRequestModal Area
      -- | DeleteSectorRequestModal Sector
    | DeleteClimbingRouteRequestModal ClimbingRoute



-- | DeleteAscentRequestModal Ascent
-- Init


init : { storageCache : String, posixTime : Int, version : String } -> Model
init { storageCache } =
    let
        decodedStorage =
            decodeString jsonFileDecoder storageCache

        jsonFile =
            Result.withDefault { climbingRoutes = Dict.empty, ascents = Dict.empty, sectors = Dict.empty, areas = Dict.empty, trips = Dict.empty } <| decodedStorage
    in
    { data = jsonFile
    , modal = Empty
    }



-- Update


type Msg
    = None
    | DeleteClimbingRouteConfirmation ClimbingRoute
    | DeleteClimbingRouteRequested ClimbingRoute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        DeleteClimbingRouteRequested route ->
            ( { model | modal = Debug.log "hierzo" DeleteClimbingRouteRequestModal route }, Cmd.none )

        DeleteClimbingRouteConfirmation route ->
            ( model, Cmd.none )



-- Utilities


ignore : ( a, b, c ) -> ( a, b )
ignore ( a, b, c ) =
    ( a, b )


withNothing : ( a, b ) -> ( a, b, Msg )
withNothing ( a, b ) =
    ( a, b, None )
