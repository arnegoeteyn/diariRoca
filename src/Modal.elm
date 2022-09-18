module Modal exposing (viewModal)

import Data exposing (Area, Ascent, ClimbingRoute, Sector, Trip)
import Date
import Dict
import Forms.Forms
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as Decode
import Message exposing (ClimbingRoutesPageMsg(..), FormMsg(..), Msg(..))
import Model exposing (ModalContent(..), Model)
import ModelAccessors as Ma
import Tailwind.Utilities as Tw
import Utilities


viewModal : Model -> Html Msg
viewModal model =
    if model.modal == Model.Empty then
        H.text ""

    else
        H.div
            [ E.onClick CloseModal
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
                , Tw.z_50
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

                    TripOverviewModal trip ->
                        tripOverviewModal model trip

                    TripFormModal ->
                        Forms.Forms.tripForm model

                    AreaFormModal ->
                        areaFormModal model

                    SectorFormModal ->
                        sectorFormModal model

                    ClimbingRouteFormModal ->
                        climbingRouteFormModal model

                    AscentFormModal ->
                        ascentFormModal model

                    DeleteAreaRequestModal area ->
                        deleteAreaConfirmation model area

                    DeleteSectorRequestModal sector ->
                        deleteSectorConfirmation model sector

                    DeleteClimbingRouteRequestModal route ->
                        deleteClimbingRouteConfirmation model route

                    DeleteAscentRequestModal ascent ->
                        deleteAscentConfirmation model ascent
                ]
            ]



--| Trip


tripOverviewModal : Model -> Trip -> Html Msg
tripOverviewModal model trip =
    let
        grades =
            Ma.getRoutesFromTrip model trip
                |> Dict.toList
                |> Utilities.sortByDescending Tuple.first
    in
    H.div
        []
        [ H.text <| Utilities.stringFromListWith " " [ Date.toIsoString trip.from, "-", Date.toIsoString trip.to ]
        , H.div []
            (List.map
                (\( grade, count ) ->
                    H.div []
                        [ H.text <|
                            Utilities.stringFromListWith
                                " "
                                [ grade, "-", String.fromInt count ]
                        ]
                )
                grades
            )
        , H.button [ E.onClick <| Message.OpenTripForm (Just trip) ] [ H.text "edit" ]
        ]



--| Area


areaFormModal : Model -> Html Msg
areaFormModal model =
    Forms.Forms.areaForm model


deleteAreaConfirmation : Model -> Area -> Html Msg
deleteAreaConfirmation model area =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete \"", area.name, "\" " ] ]
        , H.button [ E.onClick <| Message.DeleteAreaConfirmation area ] [ H.text "confirm" ]
        ]



--| Sector


sectorFormModal : Model -> Html Msg
sectorFormModal model =
    Forms.Forms.sectorForm model


deleteSectorConfirmation : Model -> Sector -> Html Msg
deleteSectorConfirmation model sector =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete \"", sector.name, "\" " ] ]
        , H.button [ E.onClick <| Message.DeleteSectorConfirmation sector ] [ H.text "confirm" ]
        ]



--| ClimbingRoute


climbingRouteFormModal : Model -> Html Msg
climbingRouteFormModal model =
    H.div []
        [ H.h2 [] [ H.text "New climbingroute" ], Forms.Forms.climbingRouteForm model ]


ascentFormModal : Model -> Html Msg
ascentFormModal model =
    H.div []
        [ H.h2 [] [ H.text "New ascent" ], Forms.Forms.ascentForm model ]


deleteClimbingRouteConfirmation : Model -> ClimbingRoute -> Html Msg
deleteClimbingRouteConfirmation _ route =
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


onClickStopPropagation : msg -> H.Attribute msg
onClickStopPropagation msg =
    E.stopPropagationOn "click" <| Decode.succeed ( msg, True )
