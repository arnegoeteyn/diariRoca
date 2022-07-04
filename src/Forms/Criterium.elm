module Forms.Criterium exposing (dateCriterium, formSelectionCriterium, formSelectionWithSearchCriterium, formTextCriterium, selectionCriterium, selectionWithSearchCriterium, textCriterium)

import Date
import DatePicker
import Forms.Form as Form exposing (Form(..), extract)
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (FormMsg, Msg(..))
import Model exposing (DateCriterium, SelectionCriterium)
import Select


textCriterium : String -> (a -> String) -> (String -> b) -> (b -> msg) -> a -> H.Html msg
textCriterium placeholder extractor wrapper toMsg value =
    H.div []
        [ H.input
            [ A.type_ "text"
            , A.placeholder placeholder
            , A.value (extractor value)
            , E.onInput (toMsg << wrapper)
            ]
            []
        ]


formTextCriterium : String -> (a -> String) -> (String -> a -> a) -> (Form a r -> FormMsg) -> Form a r -> H.Html Msg
formTextCriterium placeholder extractor wrapper toMsg form =
    textCriterium placeholder (Form.extract extractor) (\x -> Form.map (wrapper x) form) (FormMessage << toMsg) form


selectionCriterium : String -> (a -> List String) -> (String -> b) -> (b -> msg) -> String -> a -> H.Html msg
selectionCriterium placeholder extractor wrapper toMsg selected value =
    H.select
        [ E.onInput (toMsg << wrapper)
        , A.placeholder placeholder
        ]
        (List.map
            (\i ->
                H.option
                    [ A.selected (i == selected)
                    ]
                    [ H.text i ]
            )
            (extractor value)
        )


formSelectionCriterium : String -> (a -> List String) -> (String -> a -> a) -> (Form a r -> FormMsg) -> (a -> String) -> Form a r -> H.Html Msg
formSelectionCriterium placeholder extractor wrapper toMsg selectedExtractor form =
    selectionCriterium placeholder
        (Form.extract extractor)
        (\x -> Form.map (wrapper x) form)
        (FormMessage << toMsg)
        (Form.extract selectedExtractor form)
        form


selectionWithSearchCriterium : String -> Select.Config Msg item -> (a -> SelectionCriterium item) -> List item -> a -> H.Html Msg
selectionWithSearchCriterium label init extractor options value =
    let
        extracted =
            extractor value
    in
    H.div []
        [ H.fromUnstyled <|
            Select.view
                init
                (Tuple.second extracted)
                options
                (Tuple.first extracted)
        ]


formSelectionWithSearchCriterium : String -> Select.Config Msg item -> (e -> SelectionCriterium item) -> List item -> Form e r -> H.Html Msg
formSelectionWithSearchCriterium label init extractor options form =
    selectionWithSearchCriterium label init (extract extractor) options form


dateCriterium : String -> DatePicker.Settings -> (a -> DateCriterium) -> (DatePicker.Msg -> FormMsg) -> Form a r -> H.Html Msg
dateCriterium _ settings extractor toMsg form =
    let
        dateData =
            extract extractor form
    in
    DatePicker.view ((Just << Date.fromRataDie << Tuple.first) dateData) settings (Tuple.second dateData)
        |> Html.map (FormMessage << toMsg)
        |> H.fromUnstyled
