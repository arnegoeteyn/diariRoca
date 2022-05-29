module Forms.Criterium exposing (dateCriterium, selectionCriterium, selectionWithSearchCriterium, textCriterium)

import Data exposing (Area)
import Date exposing (Date)
import DatePicker
import Forms.Form as Form exposing (Form(..), extract)
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (FormMsg, Msg(..))
import Model exposing (DateCriterium, SelectionCriterium)
import Select


textCriterium : String -> (a -> String) -> (String -> a -> a) -> (Form a r -> FormMsg) -> Form a r -> H.Html Msg
textCriterium placeholder extractor wrapper toMsg form =
    H.div []
        [ H.input
            [ A.type_ "text"
            , A.placeholder placeholder
            , A.value (extract extractor form)
            , E.onInput (\x -> FormMessage << toMsg <| Form.map (wrapper x) form)
            ]
            []
        ]


selectionCriterium : String -> (b -> String) -> (String -> a -> a) -> List b -> (Form a r -> FormMsg) -> Form a r -> H.Html Msg
selectionCriterium placeholder toString wrapper options toMsg form =
    H.select
        [ E.onInput (\x -> FormMessage << toMsg <| Form.map (wrapper x) form)
        , A.placeholder placeholder
        ]
        (List.map (\i -> H.option [] [ H.text <| toString i ]) options)


selectionWithSearchCriterium : String -> Select.Config Msg item -> (e -> SelectionCriterium item) -> List item -> Form e r -> H.Html Msg
selectionWithSearchCriterium label init extractor options form =
    H.div []
        [ H.text label
        , H.fromUnstyled <|
            Select.view
                init
                (extract (extractor >> Tuple.second) form)
                options
                (extract (extractor >> Tuple.first) form)
        ]


dateCriterium : String -> DatePicker.Settings -> (a -> DateCriterium) -> (DatePicker.Msg -> FormMsg) -> Form a r -> H.Html Msg
dateCriterium label settings extractor toMsg form =
    let
        dateData =
            extract extractor form
    in
    DatePicker.view ((Just << Date.fromRataDie << Tuple.first) dateData) settings (Tuple.second dateData)
        |> Html.map (FormMessage << toMsg)
        |> H.fromUnstyled
