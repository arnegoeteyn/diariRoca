module View.Button exposing (ButtonKind(..), addButton, defaultOptions, deleteButton, editButton, gotoButton, withHref, withKind, withMsg)

import Css
import FontAwesome as Icon
import FontAwesome.Solid as Icon
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Tailwind.Utilities as Tw


type alias ButtonOptions msg =
    { kind : ButtonKind
    , msg : Maybe msg
    , href : Maybe String
    }


type ButtonKind
    = Text
    | Icon
    | TextAndIcon


defaultOptions =
    { kind = Icon
    , msg = Nothing
    , href = Nothing
    }


withKind kind options =
    { options | kind = kind }


withHref href options =
    { options | href = Just href }


withMsg : msg -> ButtonOptions msg -> ButtonOptions msg
withMsg msg options =
    { options | msg = Just msg }



--| Defaults


iconButtonCss =
    A.css
        [ Tw.text_purple_700
        , Tw.bg_gray_200
        , Tw.border
        , Tw.border_solid
        , Tw.border_purple_700
        , Tw.font_medium
        , Tw.rounded_lg
        , Tw.text_sm
        , Tw.p_1_dot_5
        , Tw.text_center
        , Tw.inline_flex
        , Tw.items_center
        , Tw.mr_2
        , Tw.gap_x_1_dot_5
        , Css.focus
            [ Tw.ring_4
            , Tw.outline_none
            , Tw.ring_purple_300
            ]
        , Css.hover
            [ Tw.bg_purple_700
            , Tw.text_white
            ]
        ]


iconButton : ButtonOptions msg -> Icon.Icon hasId -> String -> Html msg
iconButton options defaultIcon defaultText =
    let
        icon =
            Icon.view defaultIcon |> H.fromUnstyled

        text =
            H.text defaultText

        element =
            case options.href of
                Just href ->
                    H.a [ A.href href, iconButtonCss ]

                Nothing ->
                    H.button
                        [ Maybe.map E.onClick options.msg |> Maybe.withDefault (A.css [ Tw.cursor_not_allowed ])
                        , iconButtonCss
                        ]
    in
    element
        (case options.kind of
            Text ->
                [ text ]

            Icon ->
                [ icon ]

            TextAndIcon ->
                [ text, H.text "", Debug.log "icon" icon ]
        )



--| Buttons


addButton : ButtonOptions msg -> Html msg
addButton options =
    iconButton options Icon.plus "Add"


deleteButton : ButtonOptions msg -> Html msg
deleteButton options =
    iconButton options Icon.trash "Delete"


editButton : ButtonOptions msg -> Html msg
editButton options =
    iconButton options Icon.pencil "Edit"


gotoButton : ButtonOptions msg -> Html msg
gotoButton options =
    iconButton options Icon.arrowRight "Goto"
