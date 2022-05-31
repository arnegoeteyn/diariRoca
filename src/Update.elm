module Update exposing (updateWithStorage)

import Browser.Dom
import Command
import Data exposing (Ascent, ClimbingRoute, encodedJsonFile, jsonFileDecoder)
import Date
import DatePicker exposing (DateEvent(..))
import Dict
import File
import File.Download
import File.Select
import Forms.Form as Form exposing (mapAndReturn)
import Forms.Forms exposing (newId)
import Init exposing (initAreaForm, initAscentForm, initClimbingRouteForm, initSectorForm)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRoutesPageMsg(..), FormMsg(..), Msg(..))
import Model exposing (ClimbingRoutesPageModel, ModalContent(..), Model)
import ModelAccessors as MA
import Select
import Task
import Utilities exposing (flip, replaceFirst)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Dummy ->
            ( model, Cmd.none )

        SetPage page ->
            ( { model | page = page }, Cmd.none )

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

        CloseModal ->
            ( { model | modal = Model.Empty }, Cmd.none )

        ToggleSettings ->
            ( { model | settingsOpen = not model.settingsOpen }, Cmd.none )

        -- Pages
        ClimbingRoutesPageMessage crpMsg ->
            let
                ( newCrpModel, newCrpMsg ) =
                    updateClimbingRoutesPage crpMsg model.climbingRoutesPageModel
            in
            ( { model | climbingRoutesPageModel = newCrpModel }, newCrpMsg )

        -- Data
        OpenAreaForm maybeArea ->
            let
                areaId =
                    Maybe.map .id maybeArea |> Maybe.withDefault (newId model.areas)
            in
            ( { model | modal = AreaFormModal, areaFormId = areaId, areaForm = initAreaForm }, Cmd.none )

        OpenSectorForm maybeSector ->
            let
                sectorId =
                    Maybe.map .id maybeSector |> Maybe.withDefault (newId model.sectors)
            in
            ( { model | modal = SectorFormModal, sectorFormId = sectorId, sectorForm = initSectorForm }, Cmd.none )

        OpenClimbingRouteForm maybeClimbingRoute ->
            let
                climbingRouteId =
                    Maybe.map .id maybeClimbingRoute |> Maybe.withDefault (newId model.climbingRoutes)
            in
            ( { model
                | modal = ClimbingRouteFormModal
                , climbingRouteFormId = climbingRouteId
                , climbingRouteForm = initClimbingRouteForm
              }
            , Cmd.none
            )

        OpenAscentForm maybeAscent climbingRoute ->
            let
                ( ascentForm, ascentCmd ) =
                    initAscentForm model.startUpDate maybeAscent
            in
            ( { model
                | modal = AscentFormModal
                , ascentForm = ( ascentForm, Just ( maybeAscent, climbingRoute ) )
              }
            , ascentCmd
            )

        AddMediaToRoute route ->
            let
                media =
                    Forms.Forms.mediaFromForm model

                newRoute =
                    { route | media = Maybe.map (flip (::) route.media) media |> Maybe.withDefault route.media }
            in
            ( { model | climbingRoutes = Dict.insert route.id newRoute model.climbingRoutes }, Cmd.none )

        RemoveMedia route media ->
            let
                newRoute =
                    { route | media = Utilities.removeFirst ((/=) media) route.media }
            in
            ( { model | climbingRoutes = Dict.insert route.id newRoute model.climbingRoutes }, Cmd.none )

        DeleteClimbingRouteRequested ->
            ( { model | modal = Model.DeleteClimbingRouteRequestModal }, Cmd.none )

        DeleteClimbingRouteConfirmation route ->
            ( MA.deleteRoute (closeModal model) route.id, Cmd.none )

        DeleteAscentRequested ascent ->
            ( { model | modal = Model.DeleteAscentRequestModal ascent }, Cmd.none )

        DeleteAscentConfirmation ascent ->
            ( MA.deleteAscent (closeModal model) ascent.id, Cmd.none )

        FormMessage formMessage ->
            case formMessage of
                UpdateAreaForm values ->
                    ( { model | areaForm = values }, Cmd.none )

                SaveAreaForm ->
                    let
                        ( newForm, maybeArea ) =
                            Forms.Forms.validateAreaForm model
                    in
                    ( case maybeArea of
                        Just area ->
                            { model | areaForm = newForm, modal = Empty, areas = Dict.insert area.id area model.areas }

                        _ ->
                            { model | areaForm = newForm }
                    , Cmd.none
                    )

                UpdateSectorForm values ->
                    ( { model | sectorForm = values }, Cmd.none )

                SectorFormSelectArea maybeArea ->
                    let
                        newForm =
                            \f ->
                                { f
                                    | areaId =
                                        Tuple.mapFirst
                                            (\_ -> Maybe.withDefault [] <| Maybe.map List.singleton maybeArea)
                                            f.areaId
                                }
                    in
                    ( { model | sectorForm = Form.map newForm model.sectorForm }, Cmd.none )

                SectorFormSelectAreaMsg subMsg ->
                    let
                        ( updatedForm, cmd ) =
                            mapAndReturn
                                (\values ->
                                    let
                                        ( updated, selectCmd ) =
                                            Select.update Init.sectorFormAreaSelectConfig subMsg (Tuple.second values.areaId)
                                    in
                                    ( { values | areaId = Tuple.mapSecond (\_ -> updated) values.areaId }, selectCmd )
                                )
                                model.sectorForm
                    in
                    ( { model | sectorForm = updatedForm }, cmd )

                SaveSectorForm ->
                    let
                        ( newForm, maybeSector ) =
                            Forms.Forms.validateSectorForm model
                    in
                    ( case maybeSector of
                        Just sector ->
                            { model | sectorForm = newForm, modal = Empty, sectors = Dict.insert sector.id sector model.sectors }

                        _ ->
                            { model | sectorForm = newForm }
                    , Cmd.none
                    )

                UpdateClimbingRouteForm values ->
                    ( { model | climbingRouteForm = values }, Cmd.none )

                ClimbingRouteFormSelectSector maybeSector ->
                    let
                        newForm =
                            \f ->
                                { f
                                    | sectorId =
                                        Tuple.mapFirst
                                            (\_ -> Maybe.withDefault [] <| Maybe.map List.singleton maybeSector)
                                            f.sectorId
                                }
                    in
                    ( { model | climbingRouteForm = Form.map newForm model.climbingRouteForm }, Cmd.none )

                ClimbingRouteFormSelectSectorMsg subMsg ->
                    let
                        ( updatedForm, cmd ) =
                            mapAndReturn
                                (\values ->
                                    let
                                        ( updated, selectCmd ) =
                                            Select.update Init.climbingRouteFormSectorSelectConfig subMsg (Tuple.second values.sectorId)
                                    in
                                    ( { values | sectorId = Tuple.mapSecond (\_ -> updated) values.sectorId }, selectCmd )
                                )
                                model.climbingRouteForm
                    in
                    ( { model | climbingRouteForm = updatedForm }, cmd )

                SaveClimbingRouteForm ->
                    let
                        ( newForm, maybeClimbingRoute ) =
                            Forms.Forms.validateClimbingRouteForm model

                        tag =
                            "route-" ++ String.fromInt model.climbingRouteFormId

                        task =
                            Browser.Dom.getElement tag
                                |> Task.andThen (\info -> Browser.Dom.setViewport 0 info.element.y)
                                |> Task.attempt (\_ -> Dummy)
                    in
                    ( case maybeClimbingRoute of
                        Just climbingRoute ->
                            { model | climbingRouteForm = newForm, modal = Empty, climbingRoutes = Dict.insert climbingRoute.id climbingRoute model.climbingRoutes }

                        _ ->
                            { model | climbingRouteForm = newForm }
                    , task
                    )

                -- Ascent
                UpdateAscentForm values ->
                    ( { model | ascentForm = replaceFirst values model.ascentForm }, Cmd.none )

                AscentFormToDatePicker subMsg ->
                    let
                        ( updatedForm, dateEvent ) =
                            mapAndReturn
                                (\values ->
                                    let
                                        ( updated, selectCmd ) =
                                            DatePicker.update Init.ascentFormDatePickerSettings subMsg (Tuple.second values.date)
                                    in
                                    ( { values | date = Tuple.mapSecond (\_ -> updated) values.date }, selectCmd )
                                )
                                (Tuple.first model.ascentForm)

                        newDate =
                            Form.map
                                (\values ->
                                    case dateEvent of
                                        Picked date ->
                                            { values | date = Tuple.mapFirst (\_ -> Date.toRataDie date) values.date }

                                        _ ->
                                            values
                                )
                                updatedForm
                    in
                    ( { model | ascentForm = replaceFirst newDate model.ascentForm }, Cmd.none )

                SaveAscentForm ->
                    let
                        ( newForm, maybeAscent ) =
                            Forms.Forms.validateAscentForm model
                    in
                    ( case maybeAscent of
                        Just ascent ->
                            { model
                                | ascentForm = replaceFirst newForm model.ascentForm
                                , modal = Empty
                                , ascents = Dict.insert ascent.id ascent model.ascents
                            }

                        _ ->
                            { model | ascentForm = replaceFirst newForm model.ascentForm }
                    , Cmd.none
                    )


updateClimbingRoutesPage : ClimbingRoutesPageMsg -> ClimbingRoutesPageModel -> ( ClimbingRoutesPageModel, Cmd Msg )
updateClimbingRoutesPage msg model =
    let
        newSelected maybeItem default =
            case maybeItem of
                Nothing ->
                    []

                Just item ->
                    Utilities.addIfNotPresent item default

        removeFromSelected item =
            List.filter (\c -> c /= item)
    in
    case msg of
        SetRouteFilter filter ->
            ( { model | routeFilter = filter }, Cmd.none )

        SetClimbingRouteKindFilter kind ->
            ( { model | routeKindFilter = kind }, Cmd.none )

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update Init.sectorSelectConfig subMsg model.selectState
            in
            ( { model | selectState = updated }, cmd )

        SelectSector maybeSector ->
            ( { model | selected = newSelected maybeSector model.selected }, Cmd.none )

        OnRemoveSectorSelection sector ->
            ( { model | selected = removeFromSelected sector model.selected }, Cmd.none )

        OnClimbingRouteClicked maybeClimbingRoute ->
            ( { model | selectedClimbingRoute = maybeClimbingRoute }, Cmd.none )

        SetMediaLink link ->
            ( { model | mediaLink = link }, Cmd.none )

        SetMediaLabel label ->
            ( { model | mediaLabel = label }, Cmd.none )


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



--| Utilities


closeModal : Model -> Model
closeModal m =
    { m | modal = Empty }
