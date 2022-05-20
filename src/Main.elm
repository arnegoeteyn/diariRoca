module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init exposing (init)
import Message exposing (Msg(..))
import Model exposing (Model, Page(..))
import Page.AscentsPage
import Page.ClimbingRoutesPage
import Page.StatsPage
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Update exposing (updateWithStorage)
import Utilities
import View.Navbar as Navbar


mainView : Model -> Html.Html Msg
mainView model =
    Html.div [] <|
        List.map H.toUnstyled <|
            (Navbar.view model
                :: (case model.appState of
                        Model.Ready ->
                            [ case model.page of
                                ClimbingRoutesPage ->
                                    Page.ClimbingRoutesPage.view model

                                AscentsPage ->
                                    Page.AscentsPage.view model

                                StatsPage ->
                                    Page.StatsPage.view model
                            ]

                        Model.NotReady ->
                            [ H.button [ E.onClick Message.JsonRequested ] [ H.text "Load JSON" ]
                            ]
                   )
            )


main : Program { storageCache : String, posixTime : Int } Model Msg
main =
    Browser.element
        { view = mainView
        , init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }
