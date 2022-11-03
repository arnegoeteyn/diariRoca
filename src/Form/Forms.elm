module Form.Forms exposing (..)

import Data exposing (Area, Ascent, AscentKind(..), ClimbingRouteKind(..), Sector, Trip)
import DataAccessors as DA
import DataUtilities
import DatePicker exposing (DatePicker, defaultSettings)
import Dict exposing (Dict)
import Form.Criterium exposing (formSelectionCriterium, formSelectionWithSearchCriterium, formTextAreaCriterium, formTextCriterium)
import Form.Form as Form exposing (Form(..))
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Select
import Session
import Tailwind.Utilities as Tw
import Utilities



--| Generic


type alias SelectionCriterium item =
    ( List item, Select.State )


type alias DateCriterium =
    ( Int, DatePicker )


newId : Dict.Dict Int a -> Int
newId dict =
    (Maybe.withDefault 1 <| List.head <| List.sortBy (\x -> x * -1) (Dict.keys dict)) + 1


idForForm : Dict Int { a | id : Int } -> Maybe { a | id : Int } -> Int
idForForm dict m =
    Maybe.map .id m |> Maybe.withDefault (newId dict)


viewErrors : Form a r -> H.Html msg
viewErrors form =
    H.ul [ A.css [ Tw.text_red_600 ] ] <| Form.mapErrors (\error -> H.li [] [ H.text error ]) form



--| ValidationUtilities


validateNonEmpty : (a -> String) -> String -> a -> Result String String
validateNonEmpty accessor error =
    \values ->
        if not <| String.isEmpty (accessor values) then
            Ok <| accessor values

        else
            Err error


validateOptional : (a -> String) -> a -> Result String (Maybe String)
validateOptional accessor values =
    if String.isEmpty (accessor values) then
        Ok Nothing

    else
        Just (accessor values) |> Ok



--| UpdateUtilities


updateBeta : a -> { b | beta : a } -> { b | beta : a }
updateBeta value form =
    { form | beta = value }


updateComment : a -> { b | comment : a } -> { b | comment : a }
updateComment value form =
    { form | comment = value }


updateGrade : a -> { b | grade : a } -> { b | grade : a }
updateGrade value form =
    { form | grade = value }


updateKind : a -> { b | kind : a } -> { b | kind : a }
updateKind value form =
    { form | kind = value }


updateName : a -> { b | name : a } -> { b | name : a }
updateName value form =
    { form | name = value }



-- DELETE BENEATH THIS
--| Trip
-- -- | Area
-- --| Ascent
-- ascentForm : Model -> H.Html Msg
-- ascentForm model =
--     let
--         _ =
--             Tuple.first model.ascentForm
--     in
--     -- H.form []
--     --     [ formTextCriterium "Comment" .comment updateComment UpdateAscentForm form
--     --     , formSelectionCriterium "Kind"
--     --         (\_ -> List.map Data.ascentKindToString Data.ascentKindEnum)
--     --         updateKind
--     --         UpdateAscentForm
--     --         .kind
--     --         form
--     --     , dateCriterium "Date" ascentFormDatePickerSettings .date AscentFormToDatePicker form
--     --     , H.button [ A.type_ "button", E.onClick (FormMessage SaveAscentForm) ] [ H.text "Save" ]
--     --     , viewErrors form
--     --     ]
--     H.text "todo"
-- validateAscentForm : Model -> ( AscentForm, Maybe Ascent )
-- validateAscentForm model =
--     let
--         form =
--             Tuple.first model.ascentForm
--     in
--     case Tuple.second model.ascentForm of
--         Nothing ->
--             ( form, Nothing )
--         Just ( maybeAscent, climbingRoute ) ->
--             Form.succeed ValidatedAscentFormValues form
--                 |> Form.append
--                     (\_ -> Ok <| idForForm model.ascents maybeAscent)
--                 |> Form.append
--                     (\_ -> Ok climbingRoute.id)
--                 |> Form.append
--                     (\values -> Ok <| Date.fromRataDie <| Tuple.first values.date)
--                 |> Form.append
--                     (.kind >> Data.ascentKindFromString >> Result.fromMaybe "A valid ascentKind must be selected")
--                 |> Form.append
--                     (\values ->
--                         if String.isEmpty values.comment then
--                             Ok Nothing
--                         else
--                             Just values.comment |> Ok
--                     )
--                 |> ascentFromForm
-- ascentFromForm : Model.AscentForm -> ( AscentForm, Maybe Ascent )
-- ascentFromForm form =
--     case form of
--         Valid ascentFormValues values ->
--             ( Idle values
--             , Just <|
--                 Ascent ascentFormValues.id
--                     ascentFormValues.climbingRouteId
--                     ascentFormValues.date
--                     ascentFormValues.comment
--                     ascentFormValues.kind
--             )
--         Invalid errors values ->
--             ( Invalid errors values, Nothing )
--         _ ->
--             ( form, Nothing )
--| General Utilties
-- --| UpdateUtilities
-- updateAreaId : a -> { b | areaId : a } -> { b | areaId : a }
-- updateAreaId value form =
--     { form | areaId = value }
-- updateBeta : a -> { b | beta : a } -> { b | beta : a }
-- updateBeta value form =
--     { form | beta = value }
-- updateComment : a -> { b | comment : a } -> { b | comment : a }
-- updateComment value form =
--     { form | comment = value }
-- updateCountry : a -> { b | country : a } -> { b | country : a }
-- updateCountry value form =
--     { form | country = value }
-- updateGrade : a -> { b | grade : a } -> { b | grade : a }
-- updateGrade value form =
--     { form | grade = value }
-- updateKind : a -> { b | kind : a } -> { b | kind : a }
-- updateKind value form =
--     { form | kind = value }
-- updateName : a -> { b | name : a } -> { b | name : a }
-- updateName value form =
--     { form | name = value }
-- --| Dates
-- ascentFormDatePickerSettings : DatePicker.Settings
-- ascentFormDatePickerSettings =
--     defaultSettings
