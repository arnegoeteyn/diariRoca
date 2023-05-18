module Session exposing (..)

import Command
import Data exposing (Area, Ascent, ClimbingRoute, Data, Media, Sector, Trip)
import DataAccessors as DA
import DataParser exposing (jsonFileDecoder)
import Date exposing (Date)
import Dict
import Form.Criterium exposing (updateDateCriterium)
import Json.Decode exposing (decodeString)
import Time
import Utilities



-- Model


type alias Model =
    { data : Data
    , startUpDate : Date
    , version : String
    , route : Route
    , googleDriveAuthorized : Bool
    }


type Route
    = NotFoundRoute
    | ClimbingRouteRoute
    | ClimbingRoutesRoute
    | AscentsRoute
    | SectorsRoute
    | SectorRoute
    | StatsRoute


type alias ModelEncapsulated a =
    { a | session : Model }



-- Init


init : { storageCache : String, posixTime : Int, version : String } -> Route -> Model
init { storageCache, posixTime, version } route =
    let
        decodedStorage =
            decodeString jsonFileDecoder storageCache

        jsonFile =
            Result.withDefault
                { climbingRoutes = Dict.empty
                , ascents = Dict.empty
                , sectors = Dict.empty
                , areas = Dict.empty
                , trips = Dict.empty
                }
            <|
                decodedStorage
    in
    { data = jsonFile
    , startUpDate = Date.fromPosix Time.utc (Time.millisToPosix posixTime)
    , route = route
    , version = version
    , googleDriveAuthorized = False
    }



-- Methods


assign : { a | session : Model } -> ( Model, Cmd msg ) -> ( { a | session : Model }, Cmd msg )
assign model ( session, cmd ) =
    ( { model | session = session }, cmd )


assignWithCommand : ModelEncapsulated a -> Cmd msg -> ( Model, Cmd msg ) -> ( ModelEncapsulated a, Cmd msg )
assignWithCommand encapsulatedModel cmdA ( model, cmd ) =
    ( { encapsulatedModel | session = model }, Cmd.batch [ cmd, cmdA ] )


updateSessionStorage : Model -> ( Model, Cmd msg )
updateSessionStorage model =
    ( model
    , Cmd.batch
        [ Command.storeCache (DataParser.encodedJsonFile model.data)
        ]
    )



-- General


loadJson : String -> Model -> ( Model, Cmd msg )
loadJson json model =
    let
        result =
            decodeString jsonFileDecoder json
    in
    case result of
        Ok file ->
            let
                data =
                    { climbingRoutes = file.climbingRoutes
                    , ascents = file.ascents
                    , sectors = file.sectors
                    , areas = file.areas
                    , trips = file.trips
                    }
            in
            ( { model
                | data = data
              }
            , Command.storeCache (DataParser.encodedJsonFile data)
            )

        Err _ ->
            ( model, Cmd.none )



-- Area


deleteArea : Area -> Model -> ( Model, Cmd msg )
deleteArea area model =
    updateSessionStorage { model | data = DA.deleteArea area.id model.data }


addArea : Area -> Model -> ( Model, Cmd msg )
addArea area model =
    updateSessionStorage { model | data = DA.addArea area model.data }



-- Sector


deleteSector : Sector -> Model -> ( Model, Cmd msg )
deleteSector sector model =
    updateSessionStorage { model | data = DA.deleteSector sector.id model.data }


addSector : Sector -> Model -> ( Model, Cmd msg )
addSector sector model =
    updateSessionStorage { model | data = DA.addSector sector model.data }



-- Climbing Routes


addClimbingRoute : ClimbingRoute -> Model -> ( Model, Cmd msg )
addClimbingRoute climbingRoute model =
    updateSessionStorage { model | data = DA.addClimbingRoute climbingRoute model.data }


deleteClimbingRoute : ClimbingRoute -> Model -> ( Model, Cmd msg )
deleteClimbingRoute climbingRoute model =
    updateSessionStorage { model | data = DA.deleteRoute climbingRoute.id model.data }



-- Climbing Routes - Media


addMediaToRoute : ClimbingRoute -> Media -> Model -> ( Model, Cmd msg )
addMediaToRoute route media model =
    let
        newRoute =
            { route | media = media :: route.media }

        newData data =
            { data | climbingRoutes = Dict.insert route.id newRoute data.climbingRoutes }
    in
    updateSessionStorage { model | data = newData model.data }


removeMediaFromRoute : ClimbingRoute -> Media -> Model -> ( Model, Cmd msg )
removeMediaFromRoute route media model =
    let
        newRoute =
            { route | media = Utilities.removeFirst ((/=) media) route.media }

        newData data =
            { data | climbingRoutes = Dict.insert route.id newRoute data.climbingRoutes }
    in
    updateSessionStorage { model | data = newData model.data }



-- Ascents


addAscent : Ascent -> Model -> ( Model, Cmd msg )
addAscent ascent model =
    updateSessionStorage { model | data = DA.addAscent ascent model.data }


deleteAscent : Ascent -> Model -> ( Model, Cmd msg )
deleteAscent ascent model =
    updateSessionStorage { model | data = DA.deleteAscent ascent.id model.data }



-- Trips


addTrip : Trip -> Model -> ( Model, Cmd msg )
addTrip trip model =
    updateSessionStorage { model | data = DA.addTrip trip model.data }
