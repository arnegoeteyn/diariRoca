module Forms.Forms exposing (..)

import Data exposing (Area, Ascent, AscentKind(..), ClimbingRoute, ClimbingRouteKind(..), Media, Sector)
import Dict
import Form exposing (Form)
import Forms.Criterium exposing (selectionWithSearch, textCriterium)
import Forms.Form as Form exposing (Form(..), invalidateField)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init
import Message exposing (FormMsg(..), Msg(..))
import Model exposing (AreaForm, AreaFormValues, ClimbingRouteFormValues, Model, SectorForm, SectorFormValues, ValidatedSectorFormValues, ValidatedSectorFormValuesConstructor)
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
    Form.initValidate AreaFormValues model.areaForm
        |> Form.validate2
            (validateNonEmpty .name "Area can't have an empty name")
        |> Form.validate2
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
        , selectionWithSearch "Area" Init.sectorFormAreaSelectConfig .areaId (Dict.values model.areas) model.sectorForm
        , H.button [ A.type_ "button", E.onClick (FormMessage SaveSectorForm) ] [ H.text "Save" ]
        , viewErrors model.sectorForm
        ]


validateSectorForm : Model -> ( SectorForm, Maybe Sector )
validateSectorForm model =
    Form.initValidate ValidatedSectorFormValues model.sectorForm
        |> Form.validate2
            (validateNonEmpty .name "Sector can't have an empty name")
        |> Form.validate2
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



-- |> validate2 (\a -> Ok (\x -> x "1"))
-- Form.succeed
--     (\name country _ ->
--         (FormMessage << SaveAreaForm) <| areaFromFormValues { name = name, country = country } model.areaFormId
--     )
--     |> Form.append nameField
--     |> Form.append countryField
--     |> Form.append testField
--     |> Form.section "New Sector"
-- areaFromFormValues : AreaFormValues -> Int -> Area
-- areaFromFormValues values id =
--     { name = values.name
--     , country = values.country
--     , id = id
--     }
--| ClimbingRoute
-- let
--     gradeField =
--         Form.textField
--             { parser = Ok
--             , value = .grade
--             , update = \value values -> { values | grade = value }
--             , error = always Nothing
--             , attributes =
--                 { label = "Grade", placeholder = "Grade of the route" }
--             }
-- in
-- Form.succeed
--     (\name grade ->
--         (FormMessage << NewClimbingRoute) <|
--             climbingRouteFromForm2 model
--                 { name = name
--                 , grade = grade
--                 }
--     )
--     |> Form.append nameField
--     |> Form.append gradeField
--     |> Form.section "New Route"


climbingRouteFromForm2 : Model -> ClimbingRouteFormValues -> ClimbingRoute
climbingRouteFromForm2 model values =
    { id = -1
    , sectorId = -1
    , name = values.name
    , grade = values.grade
    , comment = Nothing
    , kind = Sport
    , media = []
    }


climbingRouteFromForm : Model -> ClimbingRoute
climbingRouteFromForm model =
    let
        form =
            model.climbingRoutesPageModel.climbingRouteForm

        id =
            Maybe.withDefault (newId model.climbingRoutes) model.climbingRoutesPageModel.climbingRouteForm.id

        sectorId =
            List.head form.selected |> Maybe.map .id |> Maybe.withDefault 1
    in
    { name = Maybe.withDefault "" form.name
    , grade = Maybe.withDefault "" form.grade
    , comment = form.comment
    , media = []
    , sectorId = sectorId
    , id = id
    , kind = Maybe.withDefault Sport form.kind
    }


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


updateName : a -> { b | name : a } -> { b | name : a }
updateName value form =
    { form | name = value }


updateCountry : a -> { b | country : a } -> { b | country : a }
updateCountry value form =
    { form | country = value }



-- TODO REMOVE


updateGrade : { a | grade : b } -> b -> { a | grade : b }
updateGrade form value =
    { form | grade = value }


updateComment : { a | comment : b } -> b -> { a | comment : b }
updateComment form value =
    { form | comment = value }


updateKind : { a | kind : b } -> b -> { a | kind : b }
updateKind form value =
    { form | kind = value }
