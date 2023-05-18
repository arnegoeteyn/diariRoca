module Component.ClimbingRouteList exposing (ClimbingRoutesFilter, Props, filterClimbingRoutes, initClimbingRoutesFilter, viewClimbingRoutesFilter, viewRoutes)

import Data exposing (Ascent, ClimbingRoute, Sector)
import Form.Criterium as Criterium
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Tailwind.Utilities as Tw
import View.Button as Button


type alias Props msg =
    { routes : List (Item msg), filter : ClimbingRoutesFilter }


type alias Item msg =
    { route : ClimbingRoute
    , sector : Sector
    , ascents : List Ascent
    , deleteClimbingRouteMsg : ClimbingRoute -> msg
    }


type alias ClimbingRoutesFilter =
    { projectFilter : ProjectFilter
    }


type alias ClimbingRoutesFilterFormSettings msg =
    { onUpdate : ClimbingRoutesFilter -> msg
    }


type ProjectFilter
    = AllRoutes
    | OnlyProjects
    | OnlyNonProjects


projectFilterToString : ProjectFilter -> String
projectFilterToString p =
    case p of
        AllRoutes ->
            "All"

        OnlyProjects ->
            "Only projects"

        OnlyNonProjects ->
            "Only non-projects"


projectFilterFromString : String -> ProjectFilter
projectFilterFromString p =
    case p of
        "All" ->
            AllRoutes

        "Only projects" ->
            OnlyProjects

        _ ->
            OnlyNonProjects



--| Init


initClimbingRoutesFilter : ClimbingRoutesFilter
initClimbingRoutesFilter =
    { projectFilter = AllRoutes }



--| View


viewRoutes : Props msg -> Html msg
viewRoutes props =
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
            , viewRoutesTableBody (filterClimbingRoutes props)
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
viewRoutesTableBody { routes } =
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

        sector =
            routeItem.sector
    in
    H.tr
        [ A.css [ Tw.bg_white, Tw.border_b ]
        , A.scope "row"
        , A.id <| "route-" ++ String.fromInt route.id
        ]
        [ H.td [ cellCss ] [ H.text route.grade ]
        , H.td [ cellCss, A.css [ Tw.text_left ] ]
            [ H.div [ A.css [ Tw.font_bold ] ]
                [ H.text route.name
                , H.text " ~ "
                , H.a [ A.href ("/sectors/" ++ String.fromInt sector.id) ] [ H.text sector.name ]
                ]
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



--| Filter


viewClimbingRoutesFilter : ClimbingRoutesFilterFormSettings msg -> ClimbingRoutesFilter -> H.Html msg
viewClimbingRoutesFilter settings filter =
    Criterium.selectionCriterium "ProjectFilter"
        (\_ -> List.map projectFilterToString [ AllRoutes, OnlyProjects, OnlyNonProjects ])
        (\value -> { filter | projectFilter = projectFilterFromString value })
        settings.onUpdate
        ""
        filter



--| Filters


filterClimbingRoutes : Props msg -> Props msg
filterClimbingRoutes ({ filter, routes } as props) =
    {props | routes = routes |> List.filter (filterProjects filter.projectFilter)}


filterProjects : ProjectFilter -> Item msg -> Bool
filterProjects projectFilter route =
    case projectFilter of
        AllRoutes ->
            True

        OnlyProjects ->
            List.isEmpty route.ascents

        OnlyNonProjects ->
            not <| List.isEmpty route.ascents
