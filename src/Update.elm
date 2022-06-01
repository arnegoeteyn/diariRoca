module Update exposing (updateWithStorage)

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
import Message exposing (ClimbingRoutesPageMsg(..), FormMsg(..), Msg(..))
import Model exposing (ClimbingRoutesPageModel, DateCriterium, ModalContent(..), Model, SelectionCriterium)
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
            ( { model
                | modal = ClimbingRouteFormModal
                , climbingRouteForm = ( initClimbingRouteForm (Just model) maybeClimbingRoute, maybeClimbingRoute )
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
                            updateSelectCriteriumMsg .areaId (\x v -> { v | areaId = x }) Init.sectorFormAreaSelectConfig subMsg model.sectorForm
                    in
                    ( { model
                        | sectorForm =
                            updatedForm
                      }
                    , cmd
                    )

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

                -- Ascent
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


closeModal : Model -> Model
closeModal m =
    { m | modal = Empty }
