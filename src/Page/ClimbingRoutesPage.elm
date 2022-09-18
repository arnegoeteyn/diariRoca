module Page.ClimbingRoutesPage exposing (view)

import DataParser exposing (ClimbingRoute, ascentKindToString, climbingRouteKindEnum, climbingRouteKindFromString, climbingRouteKindToString)
import DataUtilities
import Dict
import Forms.Criterium exposing (selectionCriterium, selectionWithSearchCriterium, textCriterium)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init
import Message exposing (ClimbingRoutesPageMsg(..), Msg(..))
import Model exposing (Model)
import ModelAccessors as MA
import Tailwind.Utilities as Tw
import Utilities
import View.Button as Button
import View.Link as Link


view : Model -> Html Msg
view model =
    H.div [ A.css [ Tw.px_8 ] ]
        [ viewTableHeader model
        , viewRoutesTable model
        ]


viewTableHeader : Model -> Html Msg
viewTableHeader model =
    let
        m =
            model.climbingRoutesPageModel

        routes =
            sortedAndFilteredRoutes model

        onRouteFilter =
            Forms.Criterium.textCriterium
                "route"
                .routeFilter
                identity
                (w SetRouteFilter)
                m

        onSectorFilter =
            selectionWithSearchCriterium "Sector"
                (Init.sectorSelectConfig model)
                (\aModel -> ( aModel.selected, aModel.selectState ))
                (Dict.toList model.sectors |> List.map Tuple.second)
                m

        onKindFilter =
            selectionCriterium "Kind"
                (\_ -> "" :: List.map climbingRouteKindToString climbingRouteKindEnum)
                climbingRouteKindFromString
                (w SetClimbingRouteKindFilter)
                ""
                Nothing
    in
    H.div [ A.css [ Tw.flex, Tw.justify_between ] ]
        [ H.div [ A.css [ Tw.py_4 ] ]
            [ H.h2 [ A.css [] ]
                [ H.text <| Utilities.stringFromList [ (String.fromInt << List.length) routes, " routes " ]
                , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenClimbingRouteForm Nothing))
                ]
            , H.div [ A.css [] ]
                [ onRouteFilter
                , onSectorFilter
                , onKindFilter
                ]
            ]
        ]


viewRoutesTable : Model -> Html Msg
viewRoutesTable model =
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
            [ viewRoutesTableHeader model
            , viewRoutesTableBody model
            ]
        ]


viewRoutesTableHeader : Model -> Html Msg
viewRoutesTableHeader model =
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


viewRoutesTableBody : Model -> Html Msg
viewRoutesTableBody model =
    H.tbody []
        (List.map
            (\route ->
                viewRouteRow model route
             --  , viewRouteDetail model route
            )
            (sortedAndFilteredRoutes model)
        )


viewRouteRow : Model -> ClimbingRoute -> Html Msg
viewRouteRow model route =
    let
        sector =
            MA.getSector model route.sectorId

        sectorLink =
            Link.buttonLink (MA.getSectorAndAreaNameSafe model route.sectorId) (ClimbingRoutesPageMessage <| SelectSector sector)

        ascents =
            MA.getAscents model route

        cellCss =
            A.css
                [ Tw.py_4
                , Tw.px_6
                ]
    in
    H.tr
        [ A.css [ Tw.bg_white, Tw.border_b ]
        , A.scope "row"
        , A.id <| "route-" ++ String.fromInt route.id
        ]
        [ H.td [ cellCss ] [ H.text route.grade ]
        , H.td [ cellCss, A.css [ Tw.text_left ] ]
            [ H.div [ A.css [ Tw.font_bold ] ] [ H.text route.name ]
            , sectorLink
            ]
        , H.td [ cellCss ] [ H.text (DataParser.climbingRouteKindToString route.kind) ]
        , H.td [ cellCss ] [ (H.text << String.fromInt << List.length) ascents ]
        , H.td []
            [ Button.deleteButton
                (Button.defaultOptions
                    |> Button.withMsg (Message.DeleteClimbingRouteRequested route)
                    |> Button.withKind Button.Icon
                )
            , Button.editButton
                (Button.defaultOptions
                    |> Button.withMsg (Message.OpenClimbingRouteForm (Just route))
                    |> Button.withKind Button.Icon
                )
            , Button.gotoButton
                (Button.defaultOptions
                    |> Button.withHref ("routes/" ++ String.fromInt route.id)
                    |> Button.withKind Button.Icon
                )
            ]
        ]



--| Utilities


sortedAndFilteredRoutes : Model -> List ClimbingRoute
sortedAndFilteredRoutes model =
    let
        m =
            model.climbingRoutesPageModel

        routes =
            Dict.toList model.climbingRoutes |> List.map Tuple.second
    in
    (DataParserUtilities.filterRoutes m.routeFilter m.selected m.routeKindFilter >> DataParserUtilities.sortRoutes) routes


w : (a -> Message.ClimbingRoutesPageMsg) -> a -> Msg
w msg =
    ClimbingRoutesPageMessage << msg
