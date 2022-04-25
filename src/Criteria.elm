module Criteria exposing (..)

import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E


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
