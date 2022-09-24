module Session exposing (..)

import Command
import Data exposing (Ascent, ClimbingRoute, Data, Media)
import DataAccessors as DA
import DataParser exposing (jsonFileDecoder)
import Date exposing (Date)
import Dict
import Json.Decode exposing (decodeString)
import Time
import Utilities



-- Model


type alias Model =
    { data : Data
    , startUpDate : Date
    }



-- | DeleteAscentRequestModal Ascent
-- Init


init : { storageCache : String, posixTime : Int, version : String } -> Model
init { storageCache, posixTime, version } =
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
    }


assign : { a | session : Model } -> ( Model, Cmd msg ) -> ( { a | session : Model }, Cmd msg )
assign model ( session, cmd ) =
    ( { model | session = session }, cmd )


assignCommand : { a | session : Model } -> Cmd msg -> ( Model, Cmd msg ) -> ( { a | session : Model }, Cmd msg )
assignCommand model cmdA ( session, cmd ) =
    ( { model | session = session }, Cmd.batch [ cmd, cmdA ] )


updateSessionStorage : Model -> ( Model, Cmd msg )
updateSessionStorage model =
    ( model
    , Cmd.batch
        [ Command.storeCache (DataParser.encodedJsonFile model.data)
        ]
    )



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
