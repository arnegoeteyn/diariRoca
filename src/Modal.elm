module Modal exposing (..)

import Criteria
import Data exposing (Ascent, ascentKindFromString, ascentKindToString, climbingRouteKindFromString, climbingRouteKindToString, enumAscentKind, enumClimbingRouteKind)
import Dict
import Form exposing (updateComment, updateGrade, updateKind, updateName)
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init
import Json.Decode as Decode
import Message exposing (Msg(..))
import Model exposing (ModalContent(..), Model)
import Select
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


climbingRouteFormModal : Model -> Html Msg
climbingRouteFormModal model =
    H.div []
        [ H.h2 []
            [ H.text "New climbingroute" ]
        , H.form [ A.css [ Tw.flex_col ] ] <|
            List.map (\x -> H.div [] [ x ])
                [ Criteria.viewMaybeTextInput "name"
                    model.climbingRouteForm.name
                    (\x ->
                        UpdateClimbingRouteForm <|
                            updateName model.climbingRouteForm x
                    )
                , Criteria.viewMaybeTextInput "grade"
                    model.climbingRouteForm.grade
                    (\x ->
                        UpdateClimbingRouteForm <|
                            updateGrade model.climbingRouteForm x
                    )
                , Criteria.viewMaybeTextInput "comment"
                    model.climbingRouteForm.comment
                    (\x ->
                        UpdateClimbingRouteForm <|
                            updateComment model.climbingRouteForm x
                    )
                , H.fromUnstyled <|
                    Select.view
                        Init.formSectorSelectConfig
                        model.climbingRouteForm.selectState
                        (Dict.toList model.sectors |> List.map Tuple.second)
                        model.climbingRouteForm.selected
                , Criteria.criteriaViewSelection (Nothing :: List.map Just enumClimbingRouteKind)
                    "kind"
                    (\k ->
                        case k of
                            Nothing ->
                                ""

                            Just x ->
                                climbingRouteKindToString x
                    )
                    climbingRouteKindFromString
                    (\s ->
                        UpdateClimbingRouteForm <| updateKind model.climbingRouteForm s
                    )
                ]
                ++ [ H.button [ E.onClick SaveClimbingRouteForm, A.type_ "button" ] [ H.text "Create route" ]
                   ]
        ]


deleteClimbingRouteConfirmation : Model -> Html Msg
deleteClimbingRouteConfirmation model =
    case model.selectedClimbingRoute of
        Nothing ->
            H.text "Trying to delete something that doesn't exist!"

        Just route ->
            H.div []
                [ H.h2 []
                    [ H.text <| Utilities.stringFromList [ "Delete \"", route.name, "\" " ] ]
                , H.button [ E.onClick <| Message.DeleteClimbingRouteConfirmation route ] [ H.text "confirm" ]
                ]


deleteAscentConfirmation : Model -> Ascent -> Html Msg
deleteAscentConfirmation model ascent =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete" ] ]
        , H.button [ E.onClick <| Message.DeleteAscentConfirmation ascent ] [ H.text "confirm" ]
        ]


ascentFormModal : Model -> Html Msg
ascentFormModal model =
    H.div []
        [ H.h2 []
            [ H.text "New ascent" ]
        , H.form [ A.css [ Tw.flex_col ] ] <|
            List.map (\x -> H.div [] [ x ])
                [ Criteria.viewMaybeTextInput "comment"
                    model.ascentForm.comment
                    (\x ->
                        UpdateAscentForm <|
                            updateComment model.ascentForm x
                    )
                , Criteria.criteriaViewSelection (Nothing :: List.map Just enumAscentKind)
                    "kind"
                    (\k ->
                        case k of
                            Nothing ->
                                ""

                            Just x ->
                                ascentKindToString x
                    )
                    ascentKindFromString
                    (\s ->
                        UpdateAscentForm <| updateKind model.ascentForm s
                    )
                , Criteria.dateCriteria model.ascentForm.date Init.ascentFormDatePickerSettings model.ascentForm.datePicker Message.ToDatePickerAscentForm
                ]
                ++ [ H.button [ E.onClick SaveAscentForm, A.type_ "button" ] [ H.text "Create ascent" ]
                   ]
        ]


onClickStopPropagation : msg -> H.Attribute msg
onClickStopPropagation msg =
    E.stopPropagationOn "click" <| Decode.succeed ( msg, True )
