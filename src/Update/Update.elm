module Update.Update exposing (updateWithStorage)

import Browser.Dom
import Command
import Data exposing (encodedJsonFile, jsonFileDecoder)
import Date
import DatePicker exposing (DateEvent(..))
import Dict
import File
import File.Download
import File.Select
import Forms.Form as Form exposing (Form)
import Forms.Forms exposing (newId)
import Init exposing (initAreaForm, initAscentForm, initClimbingRouteForm, initSectorForm)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRoutesPageMsg(..), FormMsg(..), Msg(..), SectorsPageMsg(..))
import Model exposing (ClimbingRoutesPageModel, DateCriterium, ModalContent(..), Model, SectorsPageModel, SelectionCriterium)
import ModelAccessors as MA
import Select
import Task
import Update.ClimbingRoutesPageUpdate as ClimbingRoutesPageUpdate
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
            loadContent model content

        ExportRequested ->
            let
                result =
                    encode 5 <| encodedJsonFile { climbingRoutes = model.climbingRoutes, ascents = model.ascents, sectors = model.sectors, areas = model.areas, trips = model.trips }
            in
            ( model, File.Download.string "result.json" "application/json" result )

        AuthorizeGoogleDrive ->
            ( model, Command.googleDriveCommand Command.Authorize )

        GoogleDriveResponse response ->
            case String.toLower response.type_ of
                "authorized" ->
                    ( { model | googleDriveAuthorized = True }, Cmd.none )

                "filechosen" ->
                    Maybe.map (loadContent model) response.argument |> Maybe.withDefault ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GoogleDriveJsonRequested ->
            ( model, Command.googleDriveCommand Command.ShowPicker )

        GoogleDriveExportRequested ->
            let
                result =
                    encode 5 <| encodedJsonFile { climbingRoutes = model.climbingRoutes, ascents = model.ascents, sectors = model.sectors, areas = model.areas, trips = model.trips }
            in
            ( model, Command.googleDriveCommand (Command.Save result) )

        CloseModal ->
            ( { model | modal = Model.Empty }, Cmd.none )

        ToggleSettings ->
            ( { model | settingsOpen = not model.settingsOpen }, Cmd.none )

        -- Pages
        ClimbingRoutesPageMessage crpMsg ->
            let
                ( newCrpModel, newCrpMsg ) =
                    ClimbingRoutesPageUpdate.update crpMsg model.climbingRoutesPageModel
            in
            ( { model | climbingRoutesPageModel = newCrpModel }, newCrpMsg )

        SectorsPageMessage spMsg ->
            let
                ( newSpModel, newSpMsg ) =
                    updateSectorsPage spMsg model.sectorsPageModel
            in
            ( { model | sectorsPageModel = newSpModel }, newSpMsg )

        -- Data - Trip
        OpenTripOverview maybeTrip ->
            case maybeTrip of
                Just trip ->
                    ( { model | modal = Model.TripOverviewModal trip }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        -- Data - Area
        OpenAreaForm maybeArea ->
            ( { model
                | modal = AreaFormModal
                , areaForm = ( initAreaForm maybeArea, maybeArea )
              }
            , Cmd.none
            )

        DeleteAreaRequested area ->
            ( { model | modal = Model.DeleteAreaRequestModal area }, Cmd.none )

        DeleteAreaConfirmation area ->
            ( MA.deleteArea area.id (closeModal model), Cmd.none )

        -- Data - Sector
        OpenSectorForm maybeSector ->
            ( { model | modal = SectorFormModal, sectorForm = ( initSectorForm (Just model) maybeSector, maybeSector ) }, Cmd.none )

        DeleteSectorRequested sector ->
            ( { model | modal = Model.DeleteSectorRequestModal sector }, Cmd.none )

        DeleteSectorConfirmation sector ->
            ( MA.deleteSector sector.id (closeModal model), Cmd.none )

        -- Data - ClimbingRoute
        OpenClimbingRouteForm maybeClimbingRoute ->
            ( { model
                | modal = ClimbingRouteFormModal
                , climbingRouteForm = ( initClimbingRouteForm (Just model) maybeClimbingRoute, maybeClimbingRoute )
              }
            , Cmd.none
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
            ( MA.deleteRoute route.id (closeModal model), Cmd.none )

        --| Data - Ascent
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

        DeleteAscentRequested ascent ->
            ( { model | modal = Model.DeleteAscentRequestModal ascent }, Cmd.none )

        DeleteAscentConfirmation ascent ->
            ( MA.deleteAscent ascent.id (closeModal model), Cmd.none )

        --| Form
        FormMessage formMessage ->
            case formMessage of
                UpdateAreaForm values ->
                    ( { model | areaForm = replaceFirst values model.areaForm }, Cmd.none )

                SaveAreaForm ->
                    let
                        ( newForm, maybeArea ) =
                            Forms.Forms.validateAreaForm model

                        areaForm =
                            replaceFirst newForm model.areaForm
                    in
                    ( case maybeArea of
                        Just area ->
                            { model
                                | areaForm = areaForm
                                , modal = Empty
                                , areas = Dict.insert area.id area model.areas
                            }

                        _ ->
                            { model | areaForm = areaForm }
                    , Cmd.none
                    )

                UpdateSectorForm values ->
                    ( { model | sectorForm = replaceFirst values model.sectorForm }, Cmd.none )

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
                    ( { model | sectorForm = Tuple.mapFirst (Form.map newForm) model.sectorForm }, Cmd.none )

                SectorFormSelectAreaMsg subMsg ->
                    let
                        ( updatedForm, cmd ) =
                            updateSelectCriteriumMsg .areaId (\x v -> { v | areaId = x }) Init.sectorFormAreaSelectConfig subMsg (Tuple.first model.sectorForm)
                    in
                    ( { model
                        | sectorForm =
                            replaceFirst updatedForm model.sectorForm
                      }
                    , cmd
                    )

                SaveSectorForm ->
                    let
                        ( newForm, maybeSector ) =
                            Forms.Forms.validateSectorForm model

                        sectorForm =
                            replaceFirst newForm model.sectorForm
                    in
                    ( case maybeSector of
                        Just sector ->
                            { model | sectorForm = sectorForm, modal = Empty, sectors = Dict.insert sector.id sector model.sectors }

                        _ ->
                            { model | sectorForm = sectorForm }
                    , Cmd.none
                    )

                UpdateClimbingRouteForm values ->
                    ( { model | climbingRouteForm = replaceFirst values model.climbingRouteForm }, Cmd.none )

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
                    ( { model | climbingRouteForm = Tuple.mapFirst (Form.map newForm) model.climbingRouteForm }, Cmd.none )

                ClimbingRouteFormSelectSectorMsg subMsg ->
                    let
                        ( updatedForm, cmd ) =
                            updateSelectCriteriumMsg .sectorId (\x v -> { v | sectorId = x }) Init.climbingRouteFormSectorSelectConfig subMsg (Tuple.first model.climbingRouteForm)
                    in
                    ( { model
                        | climbingRouteForm =
                            replaceFirst updatedForm model.climbingRouteForm
                      }
                    , cmd
                    )

                SaveClimbingRouteForm ->
                    let
                        ( newForm, maybeClimbingRoute ) =
                            Forms.Forms.validateClimbingRouteForm model

                        climbingRouteForm =
                            replaceFirst newForm model.climbingRouteForm
                    in
                    case maybeClimbingRoute of
                        Just climbingRoute ->
                            let
                                tag =
                                    "route-" ++ String.fromInt climbingRoute.id

                                task =
                                    Browser.Dom.getElement tag
                                        |> Task.andThen (\info -> Browser.Dom.setViewport 0 info.element.y)
                                        |> Task.attempt (\_ -> Dummy)
                            in
                            ( { model | climbingRouteForm = climbingRouteForm, modal = Empty, climbingRoutes = Dict.insert climbingRoute.id climbingRoute model.climbingRoutes }, task )

                        _ ->
                            ( { model | climbingRouteForm = climbingRouteForm }, Cmd.none )

                UpdateAscentForm values ->
                    ( { model | ascentForm = replaceFirst values model.ascentForm }, Cmd.none )

                AscentFormToDatePicker subMsg ->
                    ( { model
                        | ascentForm =
                            Tuple.mapFirst
                                (updateDateCriterium .date
                                    (\x v -> { v | date = x })
                                    Init.ascentFormDatePickerSettings
                                    subMsg
                                )
                                model.ascentForm
                      }
                    , Cmd.none
                    )

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


updateSectorsPage : SectorsPageMsg -> SectorsPageModel -> ( SectorsPageModel, Cmd Msg )
updateSectorsPage msg model =
    case msg of
        AreaSelected area ->
            ( { model | selectedArea = area }, Cmd.none )


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


updateDateCriterium : (values -> DateCriterium) -> (DateCriterium -> values -> values) -> DatePicker.Settings -> DatePicker.Msg -> Form values r -> Form values r
updateDateCriterium extractor wrapper settings msg form =
    let
        ( updatedForm, cmd ) =
            Form.mapAndReturn
                (\values ->
                    let
                        date =
                            extractor values

                        ( updated, selectCmd ) =
                            DatePicker.update settings msg (Tuple.second date)
                    in
                    ( wrapper (Tuple.mapSecond (\_ -> updated) date) values, selectCmd )
                )
                form

        newDateForm =
            Form.map
                (\values ->
                    case cmd of
                        Picked newDate ->
                            wrapper (Tuple.mapFirst (\_ -> Date.toRataDie newDate) (extractor values)) values

                        _ ->
                            values
                )
                updatedForm
    in
    newDateForm


updateSelectCriteriumMsg : (a -> SelectionCriterium item) -> (SelectionCriterium item -> a -> a) -> Select.Config msg item -> Select.Msg item -> Form a r -> ( Form a r, Cmd msg )
updateSelectCriteriumMsg extractor wrapper config msg =
    Form.mapAndReturn
        (\values ->
            let
                ( updated, selectCmd ) =
                    Select.update config msg (Tuple.second (extractor values))
            in
            ( wrapper (Tuple.mapSecond (\_ -> updated) (extractor values)) values
            , selectCmd
            )
        )


loadContent : Model -> String -> ( Model, Cmd msg )
loadContent model content =
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


closeModal : Model -> Model
closeModal m =
    { m | modal = Empty }
