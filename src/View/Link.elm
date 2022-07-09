module View.Link exposing (buttonLink)

import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as Decode
import Message exposing (Msg)
import Tailwind.Utilities as Tw


buttonLink : String -> Msg -> Html Msg
buttonLink s msg =
    H.button
        [ A.css <|
            [ Tw.text_gray_500, Tw.outline_none, Tw.bg_transparent, Tw.border_none, Tw.cursor_pointer ]
        , E.stopPropagationOn "click" <| Decode.succeed ( msg, True )
        ]
        [ H.text s ]
