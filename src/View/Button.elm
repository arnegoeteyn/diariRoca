module View.Button exposing (ButtonKind(..), addButton, defaultOptions, deleteButton, editIconButton, withKind, withMsg)

import Css
import FontAwesome as Icon
import FontAwesome.Solid as Icon
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg)
import Tailwind.Utilities as Tw


type alias ButtonOptions =
    { kind : ButtonKind
    , msg : Msg
    }


type ButtonKind
    = Text
    | Icon
    | TextAndIcon


defaultOptions =
    { kind = Icon
    , msg = Message.Dummy
    }


withKind kind options =
    { options | kind = kind }


withMsg : Msg -> ButtonOptions -> ButtonOptions
withMsg msg options =
    { options | msg = msg }



--| Defaults


iconButtonCss =
    A.css
        [ Tw.text_purple_700
        , Tw.border
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


iconButton : ButtonOptions -> Icon.Icon hasId -> String -> Html Msg
iconButton options defaultIcon defaultText =
    let
        icon =
            Icon.view defaultIcon |> H.fromUnstyled

        text =
            H.text defaultText
    in
    H.button
        [ E.onClick options.msg
        , iconButtonCss
        ]
        (case options.kind of
            Text ->
                [ text ]

            Icon ->
                [ icon ]

            TextAndIcon ->
                [ text, H.text "", icon ]
        )



--| Buttons


addButton : ButtonOptions -> Html Msg
addButton options =
    iconButton options Icon.plus "Add"


deleteButton : ButtonOptions -> Html Msg
deleteButton options =
    iconButton options Icon.trash "Delete"


editIconButton : ButtonOptions -> Html Msg
editIconButton options =
    iconButton options Icon.pencil "Edit"
