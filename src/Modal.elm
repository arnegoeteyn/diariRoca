module Modal exposing (viewModal)

import Criteria
import Criterium
import Data exposing (Ascent, ascentKindFromString, ascentKindToString, enumAscentKind)
import Dict
import Form exposing (updateComment, updateGrade, updateKind, updateName)
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init
import Json.Decode as Decode
import Message exposing (ClimbingRoutesPageMsg(..), Msg(..))
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
    let
        w msg =
            ClimbingRoutesPageMessage << msg

        m =
            model.climbingRoutesPageModel

        f =
            m.climbingRouteForm
    in
    H.div []
        [ H.h2 []
            [ H.text "New climbingroute" ]
        , H.form [ A.css [ Tw.flex_col ] ] <|
            List.map (\x -> H.div [] [ x ])
                [ Criterium.maybeTextCriterium "name"
                    f.name
                    (\x ->
                        w UpdateClimbingRouteForm <|
                            updateName f x
                    )
                , Criterium.maybeTextCriterium "grade"
                    m.climbingRouteForm.grade
                    (\x ->
                        w UpdateClimbingRouteForm <|
                            updateGrade f x
                    )
                , Criterium.maybeTextCriterium "comment"
                    m.climbingRouteForm.comment
                    (\x ->
                        w UpdateClimbingRouteForm <|
                            updateComment f x
                    )
                , H.fromUnstyled <|
                    Select.view
                        Init.formSectorSelectConfig
                        f.selectState
                        (Dict.toList model.sectors |> List.map Tuple.second)
                        f.selected
                , Criteria.climbingRouteKindCriterium (w UpdateClimbingRouteForm << updateKind f)
                ]
                ++ [ H.button [ E.onClick SaveClimbingRouteForm, A.type_ "button" ] [ H.text "Create route" ]
                   ]
        ]


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
    let
        m =
            model.climbingRoutesPageModel

        f =
            m.ascentForm

        w msg =
            ClimbingRoutesPageMessage << msg
    in
    H.div []
        [ H.h2 []
            [ H.text "New ascent" ]
        , H.form [ A.css [ Tw.flex_col ] ] <|
            List.map (\x -> H.div [] [ x ])
                [ Criterium.maybeTextCriterium "comment"
                    f.comment
                    (\x ->
                        w UpdateAscentForm <|
                            updateComment f x
                    )
                , Criterium.selectionCriterium (Nothing :: List.map Just enumAscentKind)
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
                        w UpdateAscentForm <| updateKind f s
                    )
                , Criterium.dateCriterium f.date Init.ascentFormDatePickerSettings f.datePicker (w ToDatePickerAscentForm)
                ]
                ++ [ H.button [ E.onClick SaveAscentForm, A.type_ "button" ] [ H.text "Create ascent" ]
                   ]
        ]


onClickStopPropagation : msg -> H.Attribute msg
onClickStopPropagation msg =
    E.stopPropagationOn "click" <| Decode.succeed ( msg, True )
