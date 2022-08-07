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
import Tailwind.Breakpoints as Bp
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
            , H.div [ A.css [] ] [ onRouteFilter ]
            ]

        -- , H.div []
        --     [ onRouteFilter
        --     , onSectorFilter
        --     , onKindFilter
        --     ]
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
            [ A.css
                [ Tw.w_full
                , Tw.text_sm

                -- , Tw.text_left
                -- , Tw.text_gray_500
                ]
            ]
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
                [ "Grade", "Name", "Kind", "Ascents" ]
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
        , E.onClick <| w OnClimbingRouteClicked (Just route)
        ]
        [ H.td [ cellCss ] [ H.text route.grade ]
        , H.td [ cellCss, A.css [ Tw.text_left ] ]
            [ H.div [ A.css [ Tw.font_bold ] ] [ H.text route.name ]
            , sectorLink
            ]
        , H.td [ cellCss ] [ H.text (Data.climbingRouteKindToString route.kind) ]
        , H.td [ cellCss ] [ (H.text << String.fromInt << List.length) ascents ]
        ]


viewRouteDetail : Model -> ClimbingRoute -> Html Msg
viewRouteDetail model route =
    if isSelected model route then
        H.div [ A.css [ Tw.grid, Tw.gap_4, Tw.grid_cols_3 ] ]
            [ H.div [ A.css [ Tw.flex, Tw.flex_col, Tw.justify_around, Tw.col_span_2 ] ]
                [ viewRouteInfo model route
                , case route.beta of
                    Just beta ->
                        H.details []
                            [ H.summary [] [ H.text "Beta" ]
                            , H.text beta
                            ]

                    Nothing ->
                        H.text ""
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



--| Utilities


isSelected : Model -> ClimbingRoute -> Bool
isSelected model route =
    Maybe.map .id model.climbingRoutesPageModel.selectedClimbingRoute == Just route.id


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
