module Update exposing (updateWithStorage)

import Browser.Dom
import Command
import Data exposing (Area, encodedJsonFile, jsonFileDecoder)
import DatePicker exposing (DateEvent(..))
import Dict
import File
import File.Download
import File.Select
import Forms.Form as Form exposing (mapAndReturn)
import Forms.Forms exposing (newId)
import Init exposing (initAreaForm, initSectorForm)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRoutesPageMsg(..), FormMsg(..), Msg(..))
import Model exposing (AreaFormValues, ClimbingRoutesPageModel, ModalContent(..), Model)
import ModelAccessors as MA
import Select
import Task
import Utilities exposing (flip)


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

        SetModal content ->
            ( { model | modal = content }, Cmd.none )

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

        SaveClimbingRouteForm ->
            let
                id =
                    Maybe.withDefault (newId model.climbingRoutes) model.climbingRoutesPageModel.climbingRouteForm.id

                tag =
                    "route-" ++ String.fromInt id

                task =
                    Browser.Dom.getElement tag
                        |> Task.andThen (\info -> Browser.Dom.setViewport 0 info.element.y)
                        |> Task.attempt (\_ -> Dummy)
            in
            ( closeModal { model | climbingRoutes = MA.addRouteFromForm model }, task )

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

        SaveAscentForm ->
            ( closeModal { model | ascents = MA.addAscentFromForm model }, Cmd.none )

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

                _ ->
                    ( model, Cmd.none )


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

        UpdateClimbingRouteForm form ->
            ( { model | climbingRouteForm = form }, Cmd.none )

        UpdateAscentForm form ->
            ( { model | ascentForm = form }, Cmd.none )

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

        ToDatePickerAscentForm subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update Init.ascentFormDatePickerSettings subMsg model.ascentForm.datePicker

                newDate =
                    case dateEvent of
                        Picked changedDate ->
                            Just changedDate

                        _ ->
                            model.ascentForm.date

                newForm =
                    (\f -> { f | date = newDate, datePicker = newDatePicker }) model.ascentForm
            in
            ( { model
                | ascentForm = newForm
              }
            , Cmd.none
            )

        SetMediaLink maybeLink ->
            ( { model | mediaLink = maybeLink }, Cmd.none )

        SetMediaLabel maybeLabel ->
            ( { model | mediaLabel = maybeLabel }, Cmd.none )


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
