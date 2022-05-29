module Forms.Criteria exposing (climbingRouteKindCriterium)

import Data exposing (ClimbingRouteKind, climbingRouteKindEnum, climbingRouteKindToString)
import Forms.Criterium
import Html.Styled as H


climbingRouteKindCriterium : (Maybe ClimbingRouteKind -> msg) -> H.Html msg
climbingRouteKindCriterium toMsg =
    H.text "replace"
