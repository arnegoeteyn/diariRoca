module View.Navbar exposing (view)

import Css
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Material.Icons.Round as IconsRound
import Material.Icons.Types exposing (Coloring(..))
import Message exposing (Msg(..))
import Model exposing (Model, Route(..))
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Utilities
import View.Icon exposing (iconButton)



-- view : Model -> Html Msg
-- view model =
--     let
--         navAttributes =
--             [ A.css
--                 [ Tw.flex
--                 , Tw.bg_purple_400
--                 ]
--             ]
--         isActive page =
--             model.page == page
--         navLink link caption =
--             H.li [ A.css [ Tw.block ] ]
--                 [ H.a
--                     [ A.css <|
--                         [ Tw.block, Tw.mt_4, Tw.mr_4, Tw.text_purple_200, Tw.outline_none, Tw.bg_transparent, Tw.border_none, Tw.cursor_pointer, B.lg [ Tw.inline_block, Tw.mt_0 ] ]
--                     -- ++ Utilities.filterAndReplaceList [ ( Tw.underline, isActive page, Just Tw.no_underline ) ]
--                     , A.href link
--                     ]
--                     [ H.text caption ]
--                 ]
--         links =
--             H.ul [ A.css [ Tw.flex, Tw.flex_row, Tw.items_center ] ]
--                 [ navLink "/" "Routes"
--                 , navLink "/ascents" "Ascents"
--                 , navLink "/sectors" "Sectors"
--                 , navLink "/stats" "Stats"
--                 , dropDown model
--                 ]
--     in
--     H.nav navAttributes
--         [ H.div [ A.css [ Tw.container, Tw.flex, Tw.flex_wrap ] ] [ links ] ]


view : Model -> Html Msg
view model =
    let
        logo =
            H.div [ A.css [ Tw.flex, Tw.items_center, Tw.flex_shrink_0, Tw.text_white, Tw.mr_6 ] ] [ H.text "Diari roca" ]

        links =
            H.div [ A.css [ Tw.w_full, Tw.block, Tw.flex_grow, B.lg [ Tw.flex, Tw.items_center, Tw.w_auto ] ] ]
                [ navLink ClimbingRoutesRoute { url = "/", caption = "Routes" }
                , navLink AscentsRoute { url = "/ascents", caption = "Ascents" }
                , navLink SectorsRoute { url = "/sectors", caption = "Sectors" }
                , navLink StatsRoute { url = "/stats", caption = "Stats" }
                ]

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

        isActive route =
            case ( route, model.route ) of
                ( ClimbingRoutesRoute, ClimbingRouteRoute _ ) ->
                    True

                _ ->
                    model.route == route

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            H.a
                [ A.href url
                , A.css <|
                    [ Tw.block, Tw.mt_4, Tw.mr_4, Tw.text_purple_200, B.lg [ Tw.inline_block, Tw.mt_0 ] ]
                        ++ Utilities.filterAndReplaceList [ ( Tw.underline, isActive route, Just Tw.no_underline ) ]
                ]
                [ H.text caption ]
    in
    H.nav navAttributes
        [ logo
        , links

        -- , button [ onClick JsonRequested ] [ text "Load JSON" ]
        -- , button [ onClick ExportRequested ] [ text "Save JSON" ]
        ]


dropDown : Model -> Html Msg
dropDown model =
    let
        versionLink =
            H.a
                [ A.css
                    [ Tw.block
                    , Tw.py_2
                    , Tw.px_4
                    , Tw.bg_gray_100
                    , Tw.text_gray_700
                    , Tw.text_xs
                    , Css.hover
                        [ Tw.bg_gray_600, Tw.text_white ]
                    ]
                , A.href ("https://github.com/arnegoeteyn/diariRoca/releases/tag/v" ++ model.version)
                , A.target "_blank"
                ]
                [ H.text ("v" ++ model.version) ]
    in
    H.li [ A.css [ Tw.block ] ]
        [ iconButton IconsRound.settings Message.ToggleSettings
        , H.div [ A.css [ Tw.bg_white, Tw.absolute ], A.css <| Utilities.filterList [ ( Tw.hidden, not model.settingsOpen ) ] ]
            (List.map
                (\( action, text ) ->
                    H.div
                        [ E.onClick action
                        , A.css
                            [ Tw.block
                            , Tw.py_2
                            , Tw.px_4
                            , Tw.bg_gray_100
                            , Tw.text_gray_700
                            , Css.hover
                                [ Tw.bg_gray_600, Tw.text_white ]
                            ]
                        ]
                        [ H.text text ]
                )
                ([ ( Message.JsonRequested, "Load JSON" )
                 , ( Message.ExportRequested, "Save JSON" )
                 ]
                    ++ (if model.googleDriveAuthorized then
                            [ ( Message.GoogleDriveJsonRequested, "Load JSON from Google Drive" )
                            , ( Message.GoogleDriveExportRequested, "Save JSON to Google Drive" )
                            ]

                        else
                            [ ( Message.AuthorizeGoogleDrive, "Authorize Google Drive" ) ]
                       )
                )
                ++ [ versionLink ]
            )
        ]
