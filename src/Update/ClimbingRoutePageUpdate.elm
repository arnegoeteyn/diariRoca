module Update.ClimbingRoutePageUpdate exposing (..)

import Data exposing (ClimbingRoute)
import Message exposing (ClimbingRoutePageMsg(..), Msg)
import Model exposing (ClimbingRoutePageModel, Model)


update : ClimbingRoutePageMsg -> Model -> ( ClimbingRoutePageModel, Cmd Msg )
update msg model =
    let
        crpModel =
            model.climbingRoutePageModel
    in
    case msg of
        SetMediaLink link ->
            ( { crpModel | mediaLink = link }, Cmd.none )

        SetMediaLabel label ->
            ( { crpModel | mediaLabel = label }, Cmd.none )
