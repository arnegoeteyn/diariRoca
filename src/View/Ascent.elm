module View.Ascent exposing (..)

import Css
import Data exposing (Ascent, ClimbingRoute)
import Date
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg)
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw


viewAscentRow : { ascent : Ascent, routeName : String } -> Html Msg
viewAscentRow { ascent, routeName } =
    H.div
        [ A.css [ Tw.flex ]
        ]
        [ H.div [ A.css [ Tw.w_2over6 ] ] [ H.text <| Date.toIsoString ascent.date ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text routeName ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text (Data.ascentKindToString ascent.kind) ]
        ]


viewAscentTripIndicator : Maybe Int -> Dict Int Css.Style -> Html Msg
viewAscentTripIndicator maybeTripId tripDict =
    let
        color =
            Maybe.andThen (\id -> Dict.get id tripDict) maybeTripId |> Maybe.withDefault Tw.bg_white
    in
    H.div [ A.css [ color, Tw.pl_10 ] ] []
