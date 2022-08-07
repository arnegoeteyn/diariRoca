module Page.ClimbingRoutePage exposing (..)

import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (ClimbingRoutesPageMsg(..), Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    H.text "climbingRoutePage"
