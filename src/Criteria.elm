module Criteria exposing (..)

import Date exposing (Date)
import DatePicker
import Html exposing (Html)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg(..))


viewTextInput : String -> String -> (String -> msg) -> H.Html msg
viewTextInput p v toMsg =
    H.input [ A.type_ "text", A.placeholder p, A.value v, E.onInput toMsg ] []


viewMaybeTextInput : String -> Maybe String -> (Maybe String -> msg) -> H.Html msg
viewMaybeTextInput p v toMsg =
    viewTextInput p
        (Maybe.withDefault "" v)
        (\s ->
            toMsg
                (if String.isEmpty s then
                    Nothing

                 else
                    Just s
                )
        )


criteriaViewSelection : List a -> String -> (a -> String) -> (String -> a) -> (a -> msg) -> H.Html msg
criteriaViewSelection l p toString fromString toMsg =
    H.select [ E.onInput <| (fromString >> toMsg), A.placeholder p ] (List.map (\i -> H.option [] [ H.text <| toString i ]) l)


dateCriteria : Maybe Date -> DatePicker.Settings -> DatePicker.DatePicker -> (DatePicker.Msg -> msg) -> H.Html msg
dateCriteria date settings datePicker toMsg =
    DatePicker.view date settings datePicker
        |> Html.map toMsg
        |> H.fromUnstyled
