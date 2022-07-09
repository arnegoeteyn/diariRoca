module Page.ClimbingRoutesPage exposing (view)

import Data exposing (ClimbingRoute, ascentKindToString, climbingRouteKindEnum, climbingRouteKindFromString, climbingRouteKindToString)
import DataUtilities
import Date
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
    H.div []
        [ viewFilters model
        , H.div [ A.css [], A.id "route-container" ] <|
            List.map
                (\route ->
                    H.div
                        [ A.id <| "route-" ++ String.fromInt route.id
                        , A.css [ Tw.border, Tw.border_solid, Tw.py_4 ]
                        , E.onClick <| w OnClimbingRouteClicked (Just route)
                        ]
                        [ viewRouteRow model route, viewRouteDetail model route ]
                )
                (sortedAndFilteredRoutes model)
        ]


viewFilters : Model -> Html Msg
viewFilters model =
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
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ (String.fromInt << List.length) routes, " routes " ]
            , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenClimbingRouteForm Nothing))
            ]
        , H.div []
            [ onRouteFilter
            , onSectorFilter
            , onKindFilter
            ]
        ]


viewRouteDetail : Model -> ClimbingRoute -> Html Msg
viewRouteDetail model route =
    if isSelected model route then
        H.div [ A.css [ Tw.grid, Tw.gap_4, Tw.grid_cols_3 ] ]
            [ H.div [ A.css [ Tw.flex, Tw.flex_col, Tw.justify_around, Tw.col_span_2 ] ]
                [ viewRouteInfo model route
                , viewAscentsList model route
                ]
            , H.div [ A.css [] ]
                [ viewRouteImage route
                , viewRouteMedia model route
                ]
            , H.div []
                [ Button.deleteButton (Button.defaultOptions |> Button.withMsg Message.DeleteClimbingRouteRequested |> Button.withKind Button.TextAndIcon)
                , Button.editButton (Button.defaultOptions |> Button.withMsg (Message.OpenClimbingRouteForm (Just route)) |> Button.withKind Button.TextAndIcon)
                ]
            ]

    else
        H.text ""


viewRouteInfo : Model -> ClimbingRoute -> Html Msg
viewRouteInfo _ climbingRoute =
    H.div [ A.css [] ]
        [ H.text <| Maybe.withDefault "" climbingRoute.comment
        ]


viewRouteImage : ClimbingRoute -> Html Msg
viewRouteImage _ =
    H.img [ A.src "https://www.barcelona-tourist-guide.com/images/ext/attractions/montserrat/L550/montserrat-barcelona-29.jpg", A.css [ Tw.col_auto, Tw.mx_auto ] ]
        []


viewRouteMedia : Model -> ClimbingRoute -> Html Msg
viewRouteMedia model route =
    let
        m =
            model.climbingRoutesPageModel

        hasMedia =
            (not << List.isEmpty) route.media

        addMediaInput =
            H.div []
                [ textCriterium "Link" .mediaLink identity (w SetMediaLink) m
                , textCriterium "Link" .mediaLabel identity (w SetMediaLabel) m
                , Button.addButton (Button.defaultOptions |> Button.withMsg (AddMediaToRoute route))
                ]
    in
    H.div []
        [ H.div [] [ H.text <| Utilities.stringFromList [ String.fromInt <| List.length route.media, " media:" ] ]
        , if hasMedia then
            H.ul [] <|
                List.map
                    (\media ->
                        H.li []
                            [ H.a [ A.css [ Tw.break_words ], A.href media.link, A.target "_blank" ] [ H.text media.label ]
                            , H.button [ E.onClick <| RemoveMedia route media ] [ H.text "x" ]
                            ]
                    )
                    route.media

          else
            H.text ""
        , addMediaInput
        ]


viewAscentsList : Model -> ClimbingRoute -> Html Msg
viewAscentsList model route =
    let
        ascents =
            MA.getAscents model route
    in
    H.div [ A.css [] ]
        [ H.h3 [ A.css [] ]
            [ H.text (Utilities.stringFromList [ String.fromInt <| List.length ascents, " ascents:" ])
            , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenAscentForm Nothing route))
            ]
        , H.div [ A.css [ Tw.divide_solid, Tw.divide_y_2, Tw.divide_x_0 ] ] <|
            List.map
                (\ascent ->
                    H.div [ A.css [ Tw.p_1_dot_5 ] ]
                        [ H.div [ A.css [ Tw.grid, Tw.grid_cols_3 ] ]
                            [ H.div [ A.css [] ] [ H.text <| Date.toIsoString ascent.date ]
                            , H.div [ A.css [] ] [ H.text (ascentKindToString ascent.kind) ]
                            , H.div
                                []
                                [ Button.deleteButton (Button.defaultOptions |> Button.withMsg (Message.DeleteAscentRequested ascent))
                                , Button.editButton (Button.defaultOptions |> Button.withMsg (Message.OpenAscentForm (Just ascent) route))
                                ]
                            ]
                        , H.div [ A.css [] ] [ H.text <| Maybe.withDefault "" ascent.comment ]
                        ]
                )
                ascents
        ]


isSelected : Model -> ClimbingRoute -> Bool
isSelected model route =
    Maybe.map .id model.climbingRoutesPageModel.selectedClimbingRoute == Just route.id


viewRouteRow : Model -> ClimbingRoute -> Html Msg
viewRouteRow model route =
    let
        sector =
            MA.getSector model route.sectorId

        sectorLink =
            Link.buttonLink (MA.getSectorAndAreaNameSafe model route.sectorId) (ClimbingRoutesPageMessage <| SelectSector sector)

        ascents =
            MA.getAscents model route
    in
    H.div
        [ A.css [ Tw.flex ]
        ]
        [ H.div [ A.css [ Tw.w_1over6 ] ] [ H.text route.grade ]
        , H.div [ A.css [ Tw.w_2over6 ] ]
            [ H.div [ A.css [ Tw.font_bold ] ] [ H.text route.name ]
            , sectorLink
            ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text (Data.climbingRouteKindToString route.kind) ]
        , H.div [ A.css [ Tw.w_1over6 ] ] [ (H.text << String.fromInt << List.length) ascents ]
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
    (DataUtilities.filterRoutes m.routeFilter m.selected m.routeKindFilter >> DataUtilities.sortRoutes) routes


w : (a -> Message.ClimbingRoutesPageMsg) -> a -> Msg
w msg =
    ClimbingRoutesPageMessage << msg
