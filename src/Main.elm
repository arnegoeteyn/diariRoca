module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Styled
import Html.Styled.Events
import Init exposing (init)
import Message exposing (Msg)
import Model exposing (Model)
import Update exposing (update, updateWithStorage)
import View exposing (view)


mainView : Model -> Html Msg
mainView model =
    Html.div [] <|
        List.map Html.Styled.toUnstyled <|
            case model.appState of
                Model.Ready ->
                    [ view model ]

                Model.NotReady ->
                    [ Html.Styled.button [ Html.Styled.Events.onClick Message.JsonRequested ] [ Html.Styled.text "Load JSON" ]
                    ]


main : Program { storageCache : String, posixTime : Int } Model Msg
main =
    Browser.element
        { view = mainView
        , init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }
