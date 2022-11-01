module Navbar exposing (Msg, view)

import Css
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Material.Icons.Round as IconsRound
import Material.Icons.Types exposing (Coloring(..))
import Session exposing (Route(..))
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Utilities
import View.Icon exposing (iconButton)


type Msg
    = NoOp


view : Session.Model -> Html Msg
view model =
    let
        logo =
            H.div [ A.css [ Tw.flex, Tw.items_center, Tw.flex_shrink_0, Tw.text_white, Tw.mr_6 ] ] [ H.text "Diari roca" ]

        links =
            H.div [ A.css [ Tw.flex, Tw.items_center, Tw.w_auto ] ]
                [ navLink ClimbingRoutesRoute { url = "/", caption = "Routes" }
                , navLink AscentsRoute { url = "/ascents", caption = "Ascents" }
                , navLink SectorsRoute { url = "/sectors", caption = "Sectors" }
                , navLink StatsRoute { url = "/stats", caption = "Stats" }
                ]

        navAttributes =
            [ A.css
                [ Tw.flex
                , Tw.flex_col
                , Tw.bg_purple_400
                , Tw.p_6
                , B.lg [ Tw.flex_row ]
                ]
            , A.id "navbar"
            ]

        isActive route =
            case ( route, model.route ) of
                ( ClimbingRoutesRoute, ClimbingRouteRoute ) ->
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
        , dropDown model
        ]


dropDown : Session.Model -> Html Msg
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
        -- [ iconButton IconsRound.settings Message.ToggleSettings
        [ H.div [ A.css [ Tw.bg_white, Tw.absolute ], A.css <| Utilities.filterList [ ( Tw.hidden, not model.settingsOpen ) ] ]
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
                []
             --     [ ( Message.JsonRequested, "Load JSON" )
             --  , ( Message.ExportRequested, "Save JSON" )
             --  ]
             -- ++ (if model.googleDriveAuthorized then
             --         [ ( Message.GoogleDriveJsonRequested, "Load JSON from Google Drive" )
             --         , ( Message.GoogleDriveExportRequested, "Save JSON to Google Drive" )
             --         ]
             --     else
             --         [ ( Message.AuthorizeGoogleDrive, "Authorize Google Drive" ) ]
             --    )
             -- ++ [ versionLink ]
            )
        ]
