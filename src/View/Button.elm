module View.Button exposing (addButton)

import Html.Styled as H exposing (Html)
import Html.Styled.Events as E
import Message exposing (Msg)


addButton : Msg -> Html Msg
addButton msg =
    H.button [ E.onClick msg ] [ H.text "+" ]
