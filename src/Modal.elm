module Modal exposing (viewModal)

import Data exposing (Ascent)
import Forms.Forms
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as Decode
import Message exposing (ClimbingRoutesPageMsg(..), FormMsg(..), Msg(..))
import Model exposing (ModalContent(..), Model)
import Tailwind.Utilities as Tw
import Utilities


viewModal : Model -> Html Msg
viewModal model =
    if model.modal == Model.Empty then
        H.text ""

    else
        H.div
            [ E.onClick (SetModal Model.Empty)
            , A.css
                [ Tw.absolute
                , Tw.top_0
                , Tw.right_0
                , Tw.left_0
                , Tw.bottom_0
                , Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.bg_gray_900
                , Tw.bg_opacity_75
                , Tw.overflow_y_auto
                , Tw.fixed
                ]
            ]
            [ H.div
                [ onClickStopPropagation Dummy
                , A.css
                    [ Tw.bg_white
                    , Tw.border_solid
                    , Tw.flex_col
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.p_16
                    ]
                ]
                [ case model.modal of
                    Empty ->
                        H.text ""

                    AreaFormModal ->
                        areaFormModal model

                    SectorFormModal ->
                        sectorFormModal model

                    ClimbingRouteFormModal ->
                        climbingRouteFormModal model

                    AscentFormModal ->
                        ascentFormModal model

                    DeleteClimbingRouteRequestModal ->
                        deleteClimbingRouteConfirmation model

                    DeleteAscentRequestModal ascent ->
                        deleteAscentConfirmation model ascent
                ]
            ]


areaFormModal : Model -> Html Msg
areaFormModal model =
    Forms.Forms.areaForm model


sectorFormModal : Model -> Html Msg
sectorFormModal model =
    Forms.Forms.sectorForm model


climbingRouteFormModal : Model -> Html Msg
climbingRouteFormModal model =
    H.div []
        [ H.h2 [] [ H.text "New climbingroute" ], Forms.Forms.climbingRouteForm model ]



-- ,
--  H.fromUnstyled <|
--     Form.View.asHtml
--         formConfig
--         (Forms.Forms.climbingRouteForm model)
--         model.climbingRouteForm
-- , H.form [ A.css [ Tw.flex_col ] ] <|
--     List.map (\x -> H.div [] [ x ])
--         [ Criterium.maybeTextCriterium "name"
--             f.name
--             (\x ->
--                 w UpdateClimbingRouteForm <|
--                     updateName f x
--             )
--         , Criterium.maybeTextCriterium "grade"
--             m.climbingRouteForm.grade
--             (\x ->
--                 w UpdateClimbingRouteForm <|
--                     updateGrade f x
--             )
--         , Criterium.maybeTextCriterium "comment"
--             m.climbingRouteForm.comment
--             (\x ->
--                 w UpdateClimbingRouteForm <|
--                     updateComment f x
--             )
--         , H.fromUnstyled <|
--             Select.view
--                 Init.formSectorSelectConfig
--                 f.selectState
--                 (Dict.toList model.sectors |> List.map Tuple.second)
--                 f.selected
--         , Criteria.climbingRouteKindCriterium (w UpdateClimbingRouteForm << updateKind f)
--         ]
--         ++ [ H.button [ E.onClick SaveClimbingRouteForm, A.type_ "button" ] [ H.text "Create route" ]
--            ]


deleteClimbingRouteConfirmation : Model -> Html Msg
deleteClimbingRouteConfirmation model =
    let
        m =
            model.climbingRoutesPageModel
    in
    case m.selectedClimbingRoute of
        Nothing ->
            H.text "Trying to delete something that doesn't exist!"

        Just route ->
            H.div []
                [ H.h2 []
                    [ H.text <| Utilities.stringFromList [ "Delete \"", route.name, "\" " ] ]
                , H.button [ E.onClick <| Message.DeleteClimbingRouteConfirmation route ] [ H.text "confirm" ]
                ]


deleteAscentConfirmation : Model -> Ascent -> Html Msg
deleteAscentConfirmation _ ascent =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete" ] ]
        , H.button [ E.onClick <| Message.DeleteAscentConfirmation ascent ] [ H.text "confirm" ]
        ]


ascentFormModal : Model -> Html Msg
ascentFormModal model =
    Debug.todo "Ascent modal"



-- let
--     m =
--         model.climbingRoutesPageModel
--     _ =
--         m.ascentForm
--     _ msg =
--         ClimbingRoutesPageMessage << msg
-- in
-- H.div []
--     [ H.h2 []
--         [ H.text "New ascent" ]
--     , H.form [ A.css [ Tw.flex_col ] ] <|
--         List.map (\x -> H.div [] [ x ])
--             [--      Forms.Criterium.maybeTextCriterium "comment"
--              --     f.comment
--              --     (\x ->
--              --         w UpdateAscentForm <|
--              --             updateComment f x
--              --     )
--              -- , Forms.Criterium.selectionCriterium (Nothing :: List.map Just enumAscentKind)
--              --     "kind"
--              --     (\k ->
--              --         case k of
--              --             Nothing ->
--              --                 ""
--              --             Just x ->
--              --                 ascentKindToString x
--              --     )
--              --     ascentKindFromString
--              --     (\s ->
--              --         w UpdateAscentForm <| updateKind f s
--              --     )
--              -- , Forms.Criterium.dateCriterium f.date Init.ascentFormDatePickerSettings f.datePicker (w ToDatePickerAscentForm)
--             ]
--             ++ [ H.button [ E.onClick SaveAscentForm, A.type_ "button" ] [ H.text "Create ascent" ]
--                ]
--     ]


onClickStopPropagation : msg -> H.Attribute msg
onClickStopPropagation msg =
    E.stopPropagationOn "click" <| Decode.succeed ( msg, True )
