module Session exposing (..)

import Command
import Data exposing (Ascent, ClimbingRoute, Data, Media, Trip)
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
    , settingsOpen : Bool
    }


type Route
    = NotFoundRoute
    | ClimbingRouteRoute
    | ClimbingRoutesRoute
    | AscentsRoute



-- | DeleteAscentRequestModal Ascent
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
    , settingsOpen = False
    }


assign : { a | session : Model } -> ( Model, Cmd msg ) -> ( { a | session : Model }, Cmd msg )
assign model ( session, cmd ) =
    ( { model | session = session }, cmd )


assignCommand : Cmd msg -> ( ModelEncapsulated a, Cmd msg ) -> ( ModelEncapsulated a, Cmd msg )
assignCommand cmdA ( model, cmd ) =
    ( model, Cmd.batch [ cmd, cmdA ] )


updateSessionStorage : Model -> ( Model, Cmd msg )
updateSessionStorage model =
    ( model
    , Cmd.batch
        [ Command.storeCache (DataParser.encodedJsonFile model.data)
        ]
    )


type alias ModelEncapsulated a =
    { a | session : Model }


update : ModelEncapsulated a -> (Model -> Model) -> ( ModelEncapsulated a, Cmd msg )
update encapsulated f =
    updateSessionStorage (f encapsulated.session)
        |> Tuple.mapFirst (\session -> { encapsulated | session = session })



-- Climbing Routes


addClimbingRoute : ClimbingRoute -> ModelEncapsulated a -> ( ModelEncapsulated a, Cmd msg )
addClimbingRoute climbingRoute model =
    update model
        (\m -> { m | data = DA.addClimbingRoute climbingRoute m.data })


deleteClimbingRoute : ClimbingRoute -> ModelEncapsulated a -> ( ModelEncapsulated a, Cmd msg )
deleteClimbingRoute climbingRoute model =
    update model (\m -> { m | data = DA.deleteRoute climbingRoute.id m.data })



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
