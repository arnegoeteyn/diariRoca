module Forms.Criteria exposing (climbingRouteKindCriterium)

import Data exposing (ClimbingRouteKind, climbingRouteKindToString, enumClimbingRouteKind)
import Forms.Criterium
import Html.Styled as H


climbingRouteKindCriterium : (Maybe ClimbingRouteKind -> msg) -> H.Html msg
climbingRouteKindCriterium toMsg =
    H.text "replace"



-- Debug.todo "replace"
-- Forms.Criterium.selectionCriterium (Nothing :: List.map Just enumClimbingRouteKind)
--     "kind"
--     (\k ->
--         case k of
--             Nothing ->
--                 ""
--             Just x ->
--                 climbingRouteKindToString x
--     )
--     Data.climbingRouteKindFromString
--     toMsg
