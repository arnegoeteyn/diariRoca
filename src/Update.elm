module Update exposing (..)

import Command
import Data exposing (AscentKind(..), ClimbingRouteKind(..), Trip, encodedJsonFile, jsonFileDecoder)
import File
import File.Download
import File.Select
import Init
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Message exposing (Msg(..))
import Model exposing (ModalContent(..), Model)
import ModelAccessors as MA
import Select
import Task
import Utilities


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newSelected maybeItem default =
            case maybeItem of
                Nothing ->
                    default

                Just item ->
                    Utilities.addIfNotPresent item default

        removeFromSelected item =
            List.filter (\c -> c /= item)
    in
    case msg of
        Dummy ->
            ( model, Cmd.none )

        JsonRequested ->
            ( model, File.Select.file [ "application/json" ] JsonSelected )

        JsonSelected file ->
            ( { model | appState = Model.NotReady }, Task.perform JsonLoaded (File.toString file) )

        JsonLoaded content ->
            let
                result =
                    decodeString jsonFileDecoder content
            in
            case result of
                Ok file ->
                    ( { model
                        | appState = Model.Ready
                        , climbingRoutes = file.climbingRoutes
                        , ascents = file.ascents
                        , sectors = file.sectors
                        , areas = file.areas
                        , trips = file.trips
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | appState = Model.Ready }, Cmd.none )

        ExportRequested ->
            let
                result =
                    encode 5 <| encodedJsonFile { climbingRoutes = model.climbingRoutes, ascents = model.ascents, sectors = model.sectors, areas = model.areas, trips = model.trips }
            in
            ( model, File.Download.string "result.json" "application/json" result )

        SetRouteFilter filter ->
            ( { model | routeFilter = filter }, Cmd.none )

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update Init.sectorSelectConfig subMsg model.selectState
            in
            ( { model | selectState = updated }, cmd )

        FormSelectSectorMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update Init.formSectorSelectConfig subMsg model.climbingRouteForm.selectState

                f =
                    \x -> { x | selectState = updated }
            in
            ( { model | climbingRouteForm = f model.climbingRouteForm }, cmd )

        SelectSector maybeSector ->
            ( { model | selected = newSelected maybeSector model.selected }, Cmd.none )

        OnRemoveSectorSelection sector ->
            ( { model | selected = removeFromSelected sector model.selected }, Cmd.none )

        OnClimbingRouteClicked maybeClimbingRoute ->
            ( { model | selectedClimbingRoute = maybeClimbingRoute }, Cmd.none )

        SetModal content ->
            ( { model | modal = content }, Cmd.none )

        UpdateClimbingRouteForm form ->
            ( { model | climbingRouteForm = form }, Cmd.none )

        SaveClimbingRouteForm ->
            ( { model | climbingRoutes = MA.addRouteFromForm model }, Cmd.none )

        FormSelectSector maybeSector ->
            let
                newForm =
                    \f -> { f | selected = newSelected maybeSector f.selected }
            in
            ( { model | climbingRouteForm = newForm model.climbingRouteForm }, Cmd.none )

        OnFormRemoveSectorSelection sector ->
            let
                newForm =
                    \f -> { f | selected = removeFromSelected sector f.selected }
            in
            ( { model | climbingRouteForm = newForm model.climbingRouteForm }, Cmd.none )

        Message.DeleteClimbingRouteRequested ->
            ( { model | modal = Model.DeleteClimbingRouteRequestModal }, Cmd.none )

        Message.DeleteClimbingRouteConfirmation route ->
            ( MA.deleteRoute { model | modal = Empty } route.id, Cmd.none )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch
        [ Command.storeCache
            (encodedJsonFile
                { climbingRoutes = newModel.climbingRoutes, ascents = newModel.ascents, sectors = newModel.sectors, areas = newModel.areas, trips = newModel.trips }
            )
        , cmds
        ]
    )
