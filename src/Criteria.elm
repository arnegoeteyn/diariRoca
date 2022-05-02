module Criteria exposing (..)

import Criterium
import Data exposing (ClimbingRoute, ClimbingRouteKind, climbingRouteKindToString, enumClimbingRouteKind)
import Html.Styled as H


climbingRouteKindCriterium : (Maybe ClimbingRouteKind -> msg) -> H.Html msg
climbingRouteKindCriterium toMsg =
    Criterium.selectionCriterium (Nothing :: List.map Just enumClimbingRouteKind)
        "kind"
        (\k ->
            case k of
                Nothing ->
                    ""

                Just x ->
                    climbingRouteKindToString x
        )
        Data.climbingRouteKindFromString
        toMsg
