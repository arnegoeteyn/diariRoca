module Main exposing (..)

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
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Update exposing (updateWithStorage)
import Utilities


mainView : Model -> Html.Html Msg
mainView model =
    Html.div [] <|
        List.map H.toUnstyled <|
            (viewNavBar model
                :: (case model.appState of
                        Model.Ready ->
                            [ case model.page of
                                ClimbingRoutesPage ->
                                    Page.ClimbingRoutesPage.view model

                                AscentsPage ->
                                    Page.AscentsPage.view model

                                StatsPage ->
                                    H.text "stats"
                            ]

                        Model.NotReady ->
                            [ H.button [ E.onClick Message.JsonRequested ] [ H.text "Load JSON" ]
                            ]
                   )
            )


viewNavBar : Model -> Html Msg
viewNavBar model =
    let
        navAttributes =
            [ A.css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_between
                , Tw.flex_wrap
                , Tw.bg_purple_400
                , Tw.p_6
                ]
            ]

        isActive : Page -> Bool
        isActive page =
            model.page == page

        navLink : Page -> String -> Html Msg
        navLink page caption =
            H.button
                [ E.onClick (SetPage page)
                , A.css <|
                    [ Tw.block, Tw.mt_4, Tw.mr_4, Tw.text_purple_200, Tw.outline_none, Tw.bg_transparent, Tw.border_none, Tw.cursor_pointer, B.lg [ Tw.inline_block, Tw.mt_0 ] ]
                        ++ Utilities.filterAndReplaceList [ ( Tw.underline, isActive page, Just Tw.no_underline ) ]
                ]
                [ H.text caption ]

        links =
            H.div [ A.css [ Tw.w_full, Tw.block, Tw.flex_grow, B.lg [ Tw.flex, Tw.items_center, Tw.w_auto ] ] ]
                [ navLink Model.ClimbingRoutesPage "Routes"
                , navLink Model.AscentsPage "Ascents"
                , navLink Model.StatsPage "Stats"
                , H.button [ E.onClick Message.JsonRequested ] [ H.text "Load JSON" ]
                ]
    in
    H.nav navAttributes
        [ links ]


main : Program { storageCache : String, posixTime : Int } Model Msg
main =
    Browser.element
        { view = mainView
        , init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }
