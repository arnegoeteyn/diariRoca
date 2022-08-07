module Update.ClimbingRoutesPageUpdate exposing (selectClimbingRoute, update)

import Data exposing (ClimbingRoute)
import Init
import Message exposing (ClimbingRoutesPageMsg(..), Msg)
import Model exposing (ClimbingRoutesPageModel, Model)
import Select
import Utilities


update : ClimbingRoutesPageMsg -> Model -> ( ClimbingRoutesPageModel, Cmd Msg )
update msg model =
    let
        crpModel =
            model.climbingRoutesPageModel

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
            ( { crpModel | routeFilter = filter }, Cmd.none )

        SetClimbingRouteKindFilter kind ->
            ( { crpModel | routeKindFilter = kind }, Cmd.none )

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (Init.sectorSelectConfig model) subMsg crpModel.selectState
            in
            ( { crpModel | selectState = updated }, cmd )

        SelectSector maybeSector ->
            ( { crpModel | selected = newSelected maybeSector crpModel.selected }, Cmd.none )

        OnRemoveSectorSelection sector ->
            ( { crpModel | selected = removeFromSelected sector crpModel.selected }, Cmd.none )



--| Public modifiers


selectClimbingRoute : ClimbingRoutesPageModel -> Maybe ClimbingRoute -> ( ClimbingRoutesPageModel, Cmd msg )
selectClimbingRoute model maybeClimbingRoute =
    ( { model | selectedClimbingRoute = maybeClimbingRoute }, Cmd.none )
