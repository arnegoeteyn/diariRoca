module Main exposing (main)

import Browser
import Html
import Html.Styled as H
import Html.Styled.Events as E
import Init exposing (init)
import Message exposing (Msg(..))
import Model exposing (Model, Page(..))
import Page.AscentsPage
import Page.ClimbingRoutesPage
import Page.SectorsPage
import Page.StatsPage
import Update exposing (updateWithStorage)
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

                                SectorsPage ->
                                    Page.SectorsPage.view model
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
