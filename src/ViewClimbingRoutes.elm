module ViewClimbingRoutes exposing (..)

import Criteria
import Criterium
import Data exposing (AscentKind(..), ClimbingRoute, ascentKindToString, climbingRouteKindToString)
import DataUtilities
import Date
import Dict
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init
import Message exposing (Msg(..))
import Modal
import Model exposing (Model, Page)
import ModelAccessors as MA
import Select
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Utilities


view : Model -> Html Msg
view model =
    H.div []
        [ viewFilters model
        , H.div [ A.css [] ] <|
            List.map
                (\route ->
                    H.div [ A.css [ Tw.border, Tw.border_solid, Tw.py_4 ], E.onClick <| Message.OnClimbingRouteClicked (Just route) ]
                        [ viewRouteRow model route, viewRouteDetail model route ]
                )
                (sortedAndFilteredRoutes model)
        , Modal.viewModal model
        ]


viewFilters : Model -> Html Msg
viewFilters model =
    let
        routes =
            sortedAndFilteredRoutes model

        onRouteFilter =
            Criterium.textCriterium
                "route"
                model.routeFilter
                (\value -> Message.SetRouteFilter value)

        onSectorFilter =
            H.fromUnstyled <|
                Select.view
                    Init.sectorSelectConfig
                    model.selectState
                    (Dict.toList model.sectors |> List.map Tuple.second)
                    model.selected

        onKindFilter =
            Criteria.climbingRouteKindCriterium SetClimbingRouteKindFilter
    in
    H.div []
        [ H.h2 [] [ H.text <| Utilities.stringFromList [ (String.fromInt << List.length) routes, " routes " ], viewAddButton model (Message.SetModal Model.ClimbingRouteFormModal) ]
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
                [ H.button [ E.onClick Message.DeleteClimbingRouteRequested ] [ H.text "Delete" ]
                ]
            ]

    else
        H.text ""


viewRouteInfo : Model -> ClimbingRoute -> Html Msg
viewRouteInfo model climbingRoute =
    H.div [ A.css [] ]
        [ H.text <| Maybe.withDefault "" climbingRoute.comment
        ]


viewRouteImage : ClimbingRoute -> Html Msg
viewRouteImage climbingRoute =
    H.img (A.src "https://www.barcelona-tourist-guide.com/images/ext/attractions/montserrat/L550/montserrat-barcelona-29.jpg" :: [ A.css [ Tw.col_auto, Tw.mx_auto ] ])
        []


viewRouteMedia : Model -> ClimbingRoute -> Html Msg
viewRouteMedia model route =
    let
        hasMedia =
            (not << List.isEmpty) route.media

        addMediaInput =
            Criterium.textCriterium "media" model.mediaInput (\_ -> Message.Dummy)

        addMediaButton =
            viewAddButton model ((\_ -> Message.Dummy) route)
    in
    H.div []
        [ H.div [] [ H.text <| Utilities.stringFromList [ String.fromInt <| List.length route.media, " media:" ] ]
        , if hasMedia then
            H.ul [] <| List.map (\m -> H.li [] [ H.a [ A.css [ Tw.break_words ], A.href m, A.target "_blank" ] [ H.text m ] ]) route.media

          else
            H.text ""
        , H.div []
            [ addMediaInput
            , addMediaButton
            ]
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
            , viewAddButton model (SetModal Model.AscentFormModal)
            ]
        , H.div [ A.css [ Tw.grid, Tw.grid_cols_1, Tw.divide_solid, Tw.divide_y_2, Tw.divide_x_0 ] ] <|
            List.map
                (\ascent ->
                    H.div [ A.css [ Tw.p_2 ] ]
                        [ H.div [ A.css [ Tw.flex, Tw.justify_around, Tw.flex_row ] ]
                            [ H.div [ A.css [] ] [ H.text <| Date.toIsoString ascent.date ]
                            , H.div [ A.css [] ] [ H.text (ascentKindToString ascent.kind) ]
                            ]
                        , H.div [ A.css [] ] [ H.text <| Maybe.withDefault "" ascent.comment ]
                        , H.div [] [ H.button [ E.onClick <| Message.DeleteAscentRequested ascent ] [ H.text "Delete ascent" ] ]
                        ]
                )
                ascents
        ]


isSelected : Model -> ClimbingRoute -> Bool
isSelected model route =
    model.selectedClimbingRoute == Just route


viewRouteRow : Model -> ClimbingRoute -> Html Msg
viewRouteRow model route =
    H.div
        [ A.css [ Tw.flex ]
        ]
        [ H.div [ A.css [ Tw.w_1over6 ] ] [ H.div [] [ H.text route.grade ] ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ viewRouteNameCell model route ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text (Data.climbingRouteKindToString route.kind) ]
        , H.div [ A.css [ Tw.w_1over6 ] ] [ (H.text << String.fromInt << List.length) (MA.getAscents model route) ]
        ]


viewRouteNameCell : Model -> ClimbingRoute -> Html Msg
viewRouteNameCell model route =
    let
        sector =
            MA.getSector model route.sectorId
    in
    H.div []
        [ H.div [] [ H.text route.name ]
        , H.div [] [ H.text (Maybe.withDefault "N/A" <| Maybe.map .name sector) ]
        ]



--| Generic views


viewAddButton : Model -> Msg -> Html Msg
viewAddButton model msg =
    H.button [ E.onClick msg ] [ H.text "+" ]



--| Utilities


sortedAndFilteredRoutes : Model -> List ClimbingRoute
sortedAndFilteredRoutes model =
    let
        routes =
            Dict.toList model.climbingRoutes |> List.map Tuple.second
    in
    (DataUtilities.filterRoutes model.routeFilter model.selected model.routeKindFilter >> DataUtilities.sortRoutes) routes
