module View.Navbar exposing (view)

import Css
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Material.Icons.Round as IconsRound
import Material.Icons.Types exposing (Coloring(..))
import Message exposing (Msg(..))
import Model exposing (Model)
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Utilities
import View.Icon exposing (iconButton)


view : Model -> Html Msg
view model =
    let
        navAttributes =
            [ A.css
                [ Tw.flex
                , Tw.bg_purple_400
                ]
            ]

        isActive page =
            model.page == page

        navLink page caption =
            H.li [ A.css [ Tw.block ] ]
                [ H.button
                    [ E.onClick (SetPage page)
                    , A.css <|
                        [ Tw.block, Tw.mt_4, Tw.mr_4, Tw.text_purple_200, Tw.outline_none, Tw.bg_transparent, Tw.border_none, Tw.cursor_pointer, B.lg [ Tw.inline_block, Tw.mt_0 ] ]
                            ++ Utilities.filterAndReplaceList [ ( Tw.underline, isActive page, Just Tw.no_underline ) ]
                    ]
                    [ H.text caption ]
                ]

        links =
            H.ul [ A.css [ Tw.flex, Tw.flex_row, Tw.items_center ] ]
                [ navLink Model.ClimbingRoutesPage "Routes"
                , navLink Model.AscentsPage "Ascents"
                , navLink Model.StatsPage "Stats"
                , dropDown model
                ]
    in
    H.nav navAttributes
        [ H.div [ A.css [ Tw.container, Tw.flex, Tw.flex_wrap ] ] [ links ] ]


dropDown : Model -> Html Msg
dropDown model =
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
                [ ( Message.JsonRequested, "Load JSON" )
                , ( Message.ExportRequested, "Save JSON" )
                ]
            )
        ]



-- [ H.button [ E.onClick Message.JsonRequested ] [ H.text "Load JSON" ]
-- , H.button [ E.onClick Message.ExportRequested ] [ H.text "Save JSON" ]
-- ])
