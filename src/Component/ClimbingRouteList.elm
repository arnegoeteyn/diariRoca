module Component.ClimbingRouteList exposing (viewRoutes, Props)

import Browser.Dom
import Data exposing (Ascent, ClimbingRoute, Sector)
import DataAccessors as MA
import DataUtilities
import Dict
import Form.Criterium as Criterium exposing (selectionCriterium, selectionWithSearchCriterium, textCriterium)
import Form.Form as Form
import Form.Forms.ClimbingRouteForm as ClimbingRouteForm
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Modal
import Select
import Session
import Skeleton
import Tailwind.Utilities as Tw
import Task
import Utilities
import View.Button as Button


type alias Props msg
    = List (Item msg)


type alias Item msg =
    { route : ClimbingRoute
    , sector : Sector
    , ascents : List Ascent
    , deleteClimbingRouteMsg: ClimbingRoute -> msg 
    }


viewRoutes : (Props msg) -> Html msg
viewRoutes routes =
    H.div
        [ A.css
            [ Tw.overflow_x_auto
            , Tw.relative
            , Tw.shadow_md
            , Tw.border
            , Tw.border_solid
            , Tw.rounded_md
            ]
        , A.id "route-container"
        ]
        [ H.table
            [ A.css [ Tw.w_full, Tw.text_sm ] ]
            [ viewRoutesTableHeader
            , viewRoutesTableBody routes
            ]
        ]


viewRoutesTableHeader : Html msg
viewRoutesTableHeader =
    H.thead
        [ A.css
            [ Tw.text_xs
            , Tw.text_gray_700
            , Tw.uppercase
            , Tw.bg_gray_50
            ]
        ]
        [ H.tr []
            (List.map
                (\header ->
                    H.th
                        [ A.scope "col"
                        , A.css
                            [ Tw.py_3
                            , Tw.px_6
                            ]
                        ]
                        [ H.text header ]
                )
                [ "Grade", "Name", "Kind", "Ascents", "Actions" ]
            )
        ]


viewRoutesTableBody : Props msg -> Html msg
viewRoutesTableBody routes =
    H.tbody []
        (List.map
            (\route ->
                viewRouteRow route route.deleteClimbingRouteMsg
            )
            routes
        )


viewRouteRow : Item msg -> (ClimbingRoute -> msg) -> Html msg
viewRouteRow routeItem deleteMsg =
    let
        cellCss =
            A.css
                [ Tw.py_4
                , Tw.px_6
                ]

        route =
            routeItem.route
    in
    H.tr
        [ A.css [ Tw.bg_white, Tw.border_b ]
        , A.scope "row"
        , A.id <| "route-" ++ String.fromInt route.id
        ]
        [ H.td [ cellCss ] [ H.text route.grade ]
        , H.td [ cellCss, A.css [ Tw.text_left ] ]
            [ H.div [ A.css [ Tw.font_bold ] ] [ H.text route.name ]

            -- , sectorLink
            ]
        , H.td [ cellCss ] [ H.text (Data.climbingRouteKindToString route.kind) ]
        , H.td [ cellCss ] [ (H.text << String.fromInt << List.length) routeItem.ascents ]
        , H.td []
            [ Button.deleteButton
                (Button.defaultOptions
                    |> Button.withMsg (deleteMsg route)
                    |> Button.withKind Button.Icon
                )
            , Button.editButton
                (Button.defaultOptions
                    -- |> Button.withMsg (OpenClimbingRouteForm (Just route))
                    |> Button.withKind Button.Icon
                )
            , Button.gotoButton
                (Button.defaultOptions
                    |> Button.withHref ("/routes/" ++ String.fromInt route.id)
                    |> Button.withKind Button.Icon
                )
            ]
        ]
