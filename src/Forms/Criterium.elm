module Forms.Criterium exposing (selectionWithSearch, textCriterium)

import Data exposing (Area)
import Date exposing (Date)
import DatePicker
import Forms.Form exposing (Form(..), extract, map)
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (FormMsg, Msg(..))
import Model exposing (SelectionCriterium)
import Select


textCriterium : String -> (a -> String) -> (String -> a -> a) -> (Form a r -> FormMsg) -> Form a r -> H.Html Msg
textCriterium placeholder extractor wrapper toMsg form =
    H.div []
        [ H.input
            [ A.type_ "text"
            , A.placeholder placeholder
            , A.value (extract extractor form)
            , E.onInput (\x -> FormMessage << toMsg <| map (wrapper x) form)
            ]
            []
        ]



-- maybeTextCriterium : String -> Maybe String -> (Maybe String -> msg) -> H.Html msg
-- maybeTextCriterium placeholder value toMsg =
--     textCriterium placeholder
--         (Maybe.withDefault "" value)
--         (\s ->
--             toMsg
--                 (if String.isEmpty s then
--                     Nothing
--                  else
--                     Just s
--                 )
--         )


selectionCriterium : List a -> String -> (a -> String) -> (String -> a) -> (a -> msg) -> H.Html msg
selectionCriterium options placeholder toString fromString toMsg =
    H.select [ E.onInput <| (fromString >> toMsg), A.placeholder placeholder ]
        (List.map (\i -> H.option [] [ H.text <| toString i ]) options)


selectionWithSearch : String -> Select.Config Msg item -> (e -> SelectionCriterium item) -> List item -> Form e r -> H.Html Msg
selectionWithSearch label init extractor options form =
    H.div []
        [ H.text label
        , H.fromUnstyled <|
            Select.view
                init
                (extract (extractor >> Tuple.second) form)
                options
                (extract (extractor >> Tuple.first) form)
        ]


dateCriterium : Maybe Date -> DatePicker.Settings -> DatePicker.DatePicker -> (DatePicker.Msg -> msg) -> H.Html msg
dateCriterium date settings datePicker toMsg =
    DatePicker.view date settings datePicker
        |> Html.map toMsg
        |> H.fromUnstyled
