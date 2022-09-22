module Forms.Criterium exposing (dateCriterium, formSelectionCriterium, formSelectionWithSearchCriterium, formTextAreaCriterium, formTextCriterium, selectionCriterium, selectionWithSearchCriterium, textCriterium)

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


formTextCriterium : String -> (a -> String) -> (String -> a -> a) -> (Form a r -> msg) -> Form a r -> H.Html msg
formTextCriterium placeholder extractor wrapper toMsg form =
    textCriterium placeholder (Form.extract extractor) (\x -> Form.mapValues (wrapper x) form) toMsg form


textAreaCriterium : String -> (a -> String) -> (String -> b) -> (b -> msg) -> a -> H.Html msg
textAreaCriterium placeholder extractor wrapper toMsg value =
    H.div []
        [ H.textarea
            [ A.placeholder placeholder
            , A.value (extractor value)
            , E.onInput (toMsg << wrapper)
            ]
            []
        ]


formTextAreaCriterium : String -> (a -> String) -> (String -> a -> a) -> (Form a r -> msg) -> Form a r -> H.Html msg
formTextAreaCriterium placeholder extractor wrapper toMsg form =
    textAreaCriterium placeholder (Form.extract extractor) (\x -> Form.mapValues (wrapper x) form) toMsg form


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


formSelectionCriterium : String -> (a -> List String) -> (String -> a -> a) -> (Form a r -> msg) -> (a -> String) -> Form a r -> H.Html msg
formSelectionCriterium placeholder extractor wrapper toMsg selectedExtractor form =
    selectionCriterium placeholder
        (Form.extract extractor)
        (\x -> Form.mapValues (wrapper x) form)
        toMsg
        (Form.extract selectedExtractor form)
        form


selectionWithSearchCriterium : String -> Select.Config msg item -> (a -> SelectionCriterium item) -> List item -> a -> H.Html msg
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


formSelectionWithSearchCriterium : String -> Select.Config msg item -> (e -> SelectionCriterium item) -> List item -> Form e r -> H.Html msg
formSelectionWithSearchCriterium label init extractor options form =
    selectionWithSearchCriterium label init (extract extractor) options form


dateCriterium : String -> DatePicker.Settings -> (a -> DateCriterium) -> (DatePicker.Msg -> msg) -> Form a r -> H.Html msg
dateCriterium _ settings extractor toMsg form =
    let
        dateData =
            extract extractor form
    in
    DatePicker.view ((Just << Date.fromRataDie << Tuple.first) dateData) settings (Tuple.second dateData)
        |> Html.map toMsg
        |> H.fromUnstyled
