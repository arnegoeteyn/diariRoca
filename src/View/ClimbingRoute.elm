module View.ClimbingRoute exposing (..)

import Data exposing (Ascent, ClimbingRoute)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg)
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw


viewRouteRow : { route : ClimbingRoute, sectorName : String, ascents : List Ascent } -> Html Msg
viewRouteRow { route, sectorName, ascents } =
    H.div
        [ A.css [ Tw.flex ]
        ]
        [ H.div [ A.css [ Tw.w_1over6 ] ] [ H.text route.grade ]
        , H.div [ A.css [ Tw.w_2over6 ] ]
            [ H.div [ A.css [ Tw.font_bold ] ] [ H.text route.name ]
            , H.div [] [ H.text sectorName ]
            ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text (Data.climbingRouteKindToString route.kind) ]
        , H.div [ A.css [ Tw.w_1over6 ] ] [ (H.text << String.fromInt << List.length) ascents ]
        ]
