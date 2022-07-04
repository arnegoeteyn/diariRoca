module Forms.Forms exposing (..)

import Data exposing (Area, Ascent, AscentKind(..), ClimbingRoute, ClimbingRouteKind(..), Media, Sector, ascentKindEnum, ascentKindFromString, ascentKindToString, climbingRouteKindEnum, climbingRouteKindFromString, climbingRouteKindToString)
import Date
import Dict exposing (Dict)
import Forms.Criterium exposing (dateCriterium, formSelectionCriterium, formTextCriterium, selectionWithSearchCriterium)
import Forms.Form as Form exposing (Form(..))
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init exposing (ascentFormDatePickerSettings)
import Message exposing (FormMsg(..), Msg(..))
import Model exposing (AreaForm, AreaFormValues, AscentForm, ClimbingRouteForm, Model, SectorForm, ValidatedAreaFormValues, ValidatedAscentFormValues, ValidatedClimbingRouteFormValues, ValidatedSectorFormValues)
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
    let
        form =
            Tuple.first model.areaForm
    in
    H.form []
        [ formTextCriterium "Name" .name updateName UpdateAreaForm form
        , formTextCriterium "Country" .country updateCountry UpdateAreaForm form
        , H.button [ A.type_ "button", E.onClick (FormMessage SaveAreaForm) ] [ H.text "Save" ]
        , viewErrors form
        ]


validateAreaForm : Model -> ( AreaForm, Maybe Area )
validateAreaForm model =
    let
        form =
            Tuple.first model.areaForm
    in
    Form.succeed ValidatedAreaFormValues form
        |> Form.append
            (validateNonEmpty .name "Area can't have an empty name")
        |> Form.append
            (validateNonEmpty .country "Area must belong to a country")
        |> Form.append
            (\_ -> Ok <| idForForm model.areas (Tuple.second model.areaForm))
        |> areaFromForm model


areaFromForm : Model -> AreaForm -> ( AreaForm, Maybe Area )
areaFromForm model form =
    case form of
        Valid areaValues values ->
            ( Idle values, Just <| Area areaValues.id areaValues.name areaValues.country )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| Sector


sectorForm : Model -> H.Html Msg
sectorForm model =
    let
        form =
            Tuple.first model.sectorForm
    in
    H.form []
        [ formTextCriterium "Name" .name updateName UpdateSectorForm form
        , selectionWithSearchCriterium "Area" Init.sectorFormAreaSelectConfig .areaId (Dict.values model.areas) form
        , H.button [ A.type_ "button", E.onClick (FormMessage SaveSectorForm) ] [ H.text "Save" ]
        , viewErrors form
        ]


validateSectorForm : Model -> ( SectorForm, Maybe Sector )
validateSectorForm model =
    let
        form =
            Tuple.first model.sectorForm
    in
    Form.succeed ValidatedSectorFormValues form
        |> Form.append
            (validateNonEmpty .name "Sector can't have an empty name")
        |> Form.append
            (.areaId >> Tuple.first >> List.head >> Maybe.map .id >> Result.fromMaybe "A valid area must be selected")
        |> Form.append
            (\_ -> Ok <| idForForm model.sectors (Tuple.second model.sectorForm))
        |> sectorFromForm model


sectorFromForm : Model -> Model.ValidatedSectorForm -> ( SectorForm, Maybe Sector )
sectorFromForm model form =
    case form of
        Valid sectorValues values ->
            ( Idle values, Just <| Sector sectorValues.id sectorValues.areaId sectorValues.name )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| ClimbingRoute


climbingRouteForm : Model -> H.Html Msg
climbingRouteForm model =
    let
        form =
            Tuple.first model.climbingRouteForm
    in
    H.form []
        [ formTextCriterium "Name" .name updateName UpdateClimbingRouteForm form
        , formTextCriterium "Grade" .grade updateGrade UpdateClimbingRouteForm form
        , selectionWithSearchCriterium "Sector" Init.climbingRouteFormSectorSelectConfig .sectorId (Dict.values model.sectors) form
        , formTextCriterium "Comment" .comment updateComment UpdateClimbingRouteForm form
        , formSelectionCriterium "Kind" (\_ -> List.map climbingRouteKindToString climbingRouteKindEnum) updateKind UpdateClimbingRouteForm form
        , H.button [ A.type_ "button", E.onClick (FormMessage SaveClimbingRouteForm) ] [ H.text "Save" ]
        , viewErrors form
        ]


validateClimbingRouteForm : Model -> ( ClimbingRouteForm, Maybe ClimbingRoute )
validateClimbingRouteForm model =
    let
        form =
            Tuple.first model.climbingRouteForm
    in
    Form.succeed ValidatedClimbingRouteFormValues form
        |> Form.append
            (\_ -> Ok <| idForForm model.climbingRoutes (Tuple.second model.climbingRouteForm))
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
        |> climbingRouteFromForm


climbingRouteFromForm : Model.ValidatedClimbingRouteForm -> ( ClimbingRouteForm, Maybe ClimbingRoute )
climbingRouteFromForm form =
    case form of
        Valid climbingRouteValues values ->
            ( Idle values
            , Just <|
                ClimbingRoute
                    climbingRouteValues.id
                    climbingRouteValues.sectorId
                    climbingRouteValues.name
                    climbingRouteValues.grade
                    climbingRouteValues.comment
                    climbingRouteValues.kind
                    []
            )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| Ascent


ascentForm : Model -> H.Html Msg
ascentForm model =
    let
        form =
            Tuple.first model.ascentForm
    in
    H.form []
        [ formTextCriterium "Comment" .comment updateComment UpdateAscentForm form
        , formSelectionCriterium "Kind"
            (\_ -> List.map ascentKindToString ascentKindEnum)
            updateKind
            UpdateAscentForm
            form
        , dateCriterium "Date" ascentFormDatePickerSettings .date AscentFormToDatePicker form
        , H.button [ A.type_ "button", E.onClick (FormMessage SaveAscentForm) ] [ H.text "Save" ]
        , viewErrors form
        ]


validateAscentForm : Model -> ( AscentForm, Maybe Ascent )
validateAscentForm model =
    let
        form =
            Tuple.first model.ascentForm
    in
    case Tuple.second model.ascentForm of
        Nothing ->
            ( form, Nothing )

        Just ( maybeAscent, climbingRoute ) ->
            Form.succeed ValidatedAscentFormValues form
                |> Form.append
                    (\_ -> Ok <| idForForm model.ascents maybeAscent)
                |> Form.append
                    (\_ -> Ok climbingRoute.id)
                |> Form.append
                    (\values -> Ok <| Date.fromRataDie <| Tuple.first values.date)
                |> Form.append
                    (.kind >> ascentKindFromString >> Result.fromMaybe "A valid ascentKind must be selected")
                |> Form.append
                    (\values ->
                        if String.isEmpty values.comment then
                            Ok Nothing

                        else
                            Just values.comment |> Ok
                    )
                |> ascentFromForm


ascentFromForm : Model.AscentForm -> ( AscentForm, Maybe Ascent )
ascentFromForm form =
    case form of
        Valid ascentFormValues values ->
            ( Idle values
            , Just <|
                Ascent ascentFormValues.id
                    ascentFormValues.climbingRouteId
                    ascentFormValues.date
                    ascentFormValues.comment
                    ascentFormValues.kind
            )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| Old


mediaFromForm : Model -> Maybe Media
mediaFromForm model =
    if List.any (\f -> (String.isEmpty << f) model.climbingRoutesPageModel) [ .mediaLink, .mediaLabel ] then
        Nothing

    else
        Just <| Media model.climbingRoutesPageModel.mediaLink model.climbingRoutesPageModel.mediaLabel



--| General Utilties


idForForm : Dict Int { a | id : Int } -> Maybe { a | id : Int } -> Int
idForForm dict m =
    Maybe.map .id m |> Maybe.withDefault (newId dict)



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
