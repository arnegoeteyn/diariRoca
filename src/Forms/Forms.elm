module Forms.Forms exposing (..)

import Data exposing (Area, Ascent, AscentKind(..), ClimbingRoute, ClimbingRouteKind(..), Media, Sector, climbingRouteKindEnum, climbingRouteKindFromString, climbingRouteKindToString)
import Dict
import Forms.Criterium exposing (selectionCriterium, selectionWithSearchCriterium, textCriterium)
import Forms.Form as Form exposing (Form(..), append, succeed)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init
import Message exposing (FormMsg(..), Msg(..))
import Model exposing (AreaForm, AreaFormValues, ClimbingRouteForm, ClimbingRouteFormValues, Model, SectorForm, SectorFormValues, ValidatedClimbingRouteFormValues, ValidatedSectorFormValues, ValidatedSectorFormValuesConstructor)
import Tailwind.Utilities as Tw



--| Generic


newId : Dict.Dict Int a -> Int
newId dict =
    (Maybe.withDefault 1 <| List.head <| List.sortBy (\x -> x * -1) (Dict.keys dict)) + 1


viewErrors : Form a r -> H.Html Msg
viewErrors form =
    H.ul [ A.css [ Tw.text_red_600 ] ] <| Form.mapErrors (\error -> H.li [] [ H.text error ]) form



-- | Area


areaForm : Model -> H.Html Msg
areaForm model =
    H.form []
        [ textCriterium "Name" .name updateName UpdateAreaForm model.areaForm
        , textCriterium "Country" .country updateCountry UpdateAreaForm model.areaForm
        , H.button [ A.type_ "button", E.onClick (FormMessage SaveAreaForm) ] [ H.text "Save" ]
        , viewErrors model.areaForm
        ]


validateAreaForm : Model -> ( AreaForm, Maybe Area )
validateAreaForm model =
    Form.succeed AreaFormValues model.areaForm
        |> Form.append
            (validateNonEmpty .name "Area can't have an empty name")
        |> Form.append
            (validateNonEmpty .country "Area must belong to a country")
        |> areaFromForm model


areaFromForm : Model -> AreaForm -> ( AreaForm, Maybe Area )
areaFromForm model form =
    case form of
        Valid areaValues values ->
            ( Idle values, Just <| Area model.areaFormId areaValues.name areaValues.country )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| Sector


sectorForm : Model -> H.Html Msg
sectorForm model =
    H.form []
        [ textCriterium "Name" .name updateName UpdateSectorForm model.sectorForm
        , selectionWithSearchCriterium "Area" Init.sectorFormAreaSelectConfig .areaId (Dict.values model.areas) model.sectorForm
        , H.button [ A.type_ "button", E.onClick (FormMessage SaveSectorForm) ] [ H.text "Save" ]
        , viewErrors model.sectorForm
        ]


validateSectorForm : Model -> ( SectorForm, Maybe Sector )
validateSectorForm model =
    Form.succeed ValidatedSectorFormValues model.sectorForm
        |> Form.append
            (validateNonEmpty .name "Sector can't have an empty name")
        |> Form.append
            (.areaId >> Tuple.first >> List.head >> Maybe.map .id >> Result.fromMaybe "A valid area must be selected")
        |> sectorFromForm model


sectorFromForm : Model -> Model.ValidatedSectorForm -> ( SectorForm, Maybe Sector )
sectorFromForm model form =
    case form of
        Valid sectorValues values ->
            ( Idle values, Just <| Sector model.sectorFormId sectorValues.areaId sectorValues.name )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| ClimbingRoute


climbingRouteForm : Model -> H.Html Msg
climbingRouteForm model =
    H.form []
        [ textCriterium "Name" .name updateName UpdateClimbingRouteForm model.climbingRouteForm
        , textCriterium "Grade" .grade updateGrade UpdateClimbingRouteForm model.climbingRouteForm
        , selectionWithSearchCriterium "Sector" Init.climbingRouteFormSectorSelectConfig .sectorId (Dict.values model.sectors) model.climbingRouteForm
        , textCriterium "Comment" .comment updateComment UpdateClimbingRouteForm model.climbingRouteForm
        , selectionCriterium "Kind" climbingRouteKindToString updateKind climbingRouteKindEnum UpdateClimbingRouteForm model.climbingRouteForm
        , H.button [ A.type_ "button", E.onClick (FormMessage SaveClimbingRouteForm) ] [ H.text "Save" ]
        , viewErrors model.climbingRouteForm
        ]


validateClimbingRouteForm : Model -> ( ClimbingRouteForm, Maybe ClimbingRoute )
validateClimbingRouteForm model =
    Form.succeed ValidatedClimbingRouteFormValues model.climbingRouteForm
        |> Form.append
            (validateNonEmpty .name "Route can't have an empty name")
        |> Form.append
            (validateNonEmpty .grade "Route can't have no grade")
        |> Form.append
            (\values ->
                if String.isEmpty values.comment then
                    Ok Nothing

                else
                    Just values.comment |> Ok
            )
        |> Form.append
            (.kind >> climbingRouteKindFromString >> Result.fromMaybe "A valid routeKind must be selected")
        |> Form.append
            (.sectorId >> Tuple.first >> List.head >> Maybe.map .id >> Result.fromMaybe "A valid sector must be selected")
        |> climbingRouteFromForm model
        |> Debug.log "validated"


climbingRouteFromForm : Model -> Model.ValidatedClimbingRouteForm -> ( ClimbingRouteForm, Maybe ClimbingRoute )
climbingRouteFromForm model form =
    case form of
        Valid climbingRouteValues values ->
            ( Idle values
            , Just <| ClimbingRoute model.climbingRouteFormId climbingRouteValues.sectorId climbingRouteValues.name climbingRouteValues.grade climbingRouteValues.comment climbingRouteValues.kind []
            )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| Old


mediaFromForm : Model -> Maybe Media
mediaFromForm model =
    Maybe.map2 Media model.climbingRoutesPageModel.mediaLink model.climbingRoutesPageModel.mediaLabel



--| Ascent


ascentFromForm : Model -> Ascent
ascentFromForm model =
    let
        crpModel =
            model.climbingRoutesPageModel

        form =
            crpModel.ascentForm

        id =
            Maybe.withDefault (newId model.ascents) crpModel.ascentForm.id

        routeId =
            Maybe.withDefault -1 (Maybe.map .id crpModel.selectedClimbingRoute)
    in
    { comment = form.comment
    , routeId = routeId
    , id = id
    , kind = Maybe.withDefault Redpoint form.kind
    , date = Maybe.withDefault model.startUpDate form.date
    }



--| ValidationUtilities


validateNonEmpty : (a -> String) -> String -> a -> Result String String
validateNonEmpty accessor error =
    \values ->
        if not <| String.isEmpty (accessor values) then
            Ok <| accessor values

        else
            Err error



--| UpdateUtilities


updateAreaId : a -> { b | areaId : a } -> { b | areaId : a }
updateAreaId value form =
    { form | areaId = value }


updateComment : a -> { b | comment : a } -> { b | comment : a }
updateComment value form =
    { form | comment = value }


updateCountry : a -> { b | country : a } -> { b | country : a }
updateCountry value form =
    { form | country = value }


updateGrade : a -> { b | grade : a } -> { b | grade : a }
updateGrade value form =
    { form | grade = value }


updateKind : a -> { b | kind : a } -> { b | kind : a }
updateKind value form =
    { form | kind = value }


updateName : a -> { b | name : a } -> { b | name : a }
updateName value form =
    { form | name = value }
