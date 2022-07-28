module Main exposing (main)

import Browser
import Command
import FontAwesome.Styles as Icon
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Init exposing (init)
import Message exposing (Msg(..))
import Modal
import Model exposing (Model, Page(..))
import Page.AscentsPage
import Page.ClimbingRoutesPage
import Page.SectorsPage
import Page.StatsPage
import Tailwind.Utilities as Tw
import Update.Update exposing (updateWithStorage)
import View.Navbar as Navbar


mainView : Model -> Html.Html Msg
mainView model =
    Html.div []
        [ Icon.css
        , Html.div []
            (List.map
                H.toUnstyled
                [ H.header
                    [ A.css
                        [ Tw.sticky
                        , Tw.top_0
                        , Tw.z_50
                        ]
                    , A.id "navbar"
                    ]
                    [ Navbar.view model ]
                , Modal.viewModal model
                , H.div [ A.css [ Tw.relative ] ]
                    [ case model.appState of
                        Model.Ready ->
                            case model.page of
                                ClimbingRoutesPage ->
                                    Page.ClimbingRoutesPage.view model

                                AscentsPage ->
                                    Page.AscentsPage.view model

                                StatsPage ->
                                    Page.StatsPage.view model

                                SectorsPage ->
                                    Page.SectorsPage.view model

                        Model.NotReady ->
                            H.button [ E.onClick Message.JsonRequested ] [ H.text "Load JSON" ]
                    ]
                ]
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Command.loadCache JsonLoaded, Command.googleDriveSubscriptionPort GoogleDriveResponse ]


main : Program { storageCache : String, posixTime : Int, version : String } Model Msg
main =
    Browser.element
        { view = mainView
        , init = init
        , update = updateWithStorage
        , subscriptions = subscriptions
        }
