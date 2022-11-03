module Modal exposing (view)

import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as Decode
import Tailwind.Utilities as Tw


view : msg -> msg -> Html msg -> Html msg
view close propagation child =
    H.div [ A.css [ Tw.relative ] ]
        [ H.div
            [ E.onClick close
            , A.css
                [ Tw.absolute
                , Tw.top_0
                , Tw.right_0
                , Tw.left_0
                , Tw.bottom_0
                , Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.bg_gray_900
                , Tw.bg_opacity_75
                , Tw.overflow_y_auto
                , Tw.fixed
                , Tw.z_50
                ]
            ]
            [ H.div
                [ onClickStopPropagation propagation
                , A.css
                    [ Tw.bg_white
                    , Tw.border_solid
                    , Tw.flex_col
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.p_16
                    ]
                ]
                [ child ]
            ]
        ]


onClickStopPropagation : msg -> H.Attribute msg
onClickStopPropagation msg =
    E.stopPropagationOn "click" <| Decode.succeed ( msg, True )
