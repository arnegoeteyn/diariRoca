module Update.ClimbingRoutesPageUpdate exposing (update)

import Init
import Message exposing (ClimbingRoutesPageMsg(..), Msg)
import Model exposing (ClimbingRoutesPageModel)
import Select
import Utilities


update : ClimbingRoutesPageMsg -> ClimbingRoutesPageModel -> ( ClimbingRoutesPageModel, Cmd Msg )
update msg model =
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
