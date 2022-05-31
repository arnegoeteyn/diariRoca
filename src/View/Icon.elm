module View.Icon exposing (..)

import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Material.Icons.Types exposing (Coloring(..), Icon)
import Tailwind.Utilities as Tw


iconButton : Icon msg -> msg -> Html msg
iconButton icon msg =
    H.button
        [ E.onClick msg
        , A.css
            [ Tw.bg_transparent
            , Tw.border_0
            , Css.hover
                [ Tw.bg_black, Tw.bg_opacity_10 ]
            ]
        ]
        [ H.fromUnstyled <| icon 24 Inherit ]
