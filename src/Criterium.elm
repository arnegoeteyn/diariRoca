module Criterium exposing (dateCriterium, maybeTextCriterium, selectionCriterium, textCriterium)

import Date exposing (Date)
import DatePicker
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E


textCriterium : String -> String -> (String -> msg) -> H.Html msg
textCriterium p v toMsg =
    H.input [ A.type_ "text", A.placeholder p, A.value v, E.onInput toMsg ] []


maybeTextCriterium : String -> Maybe String -> (Maybe String -> msg) -> H.Html msg
maybeTextCriterium p v toMsg =
    textCriterium p
        (Maybe.withDefault "" v)
        (\s ->
            toMsg
                (if String.isEmpty s then
                    Nothing

                 else
                    Just s
                )
        )


selectionCriterium : List a -> String -> (a -> String) -> (String -> a) -> (a -> msg) -> H.Html msg
selectionCriterium l p toString fromString toMsg =
    H.select [ E.onInput <| (fromString >> toMsg), A.placeholder p ] (List.map (\i -> H.option [] [ H.text <| toString i ]) l)


dateCriterium : Maybe Date -> DatePicker.Settings -> DatePicker.DatePicker -> (DatePicker.Msg -> msg) -> H.Html msg
dateCriterium date settings datePicker toMsg =
    DatePicker.view date settings datePicker
        |> Html.map toMsg
        |> H.fromUnstyled
