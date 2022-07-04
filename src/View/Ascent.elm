module View.Ascent exposing (viewAscentRow, viewAscentTripIndicator)

import Css
import Data exposing (Ascent, Trip)
import Date
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg)
import Tailwind.Utilities as Tw
import Utilities


viewAscentRow : { ascent : Ascent, routeName : String, routeGrade : String } -> Html Msg
viewAscentRow { ascent, routeName, routeGrade } =
    H.div
        [ A.css [ Tw.flex ]
        ]
        [ H.div [ A.css [ Tw.w_2over6 ] ] [ H.text <| Date.toIsoString ascent.date ]
        , H.div [ A.css [ Tw.w_2over6 ] ]
            [ H.text <| Utilities.stringFromList [ routeName, " ", "(", routeGrade, ")" ]
            ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text (Data.ascentKindToString ascent.kind) ]
        ]


viewAscentTripIndicator : Maybe Trip -> Dict Int Css.Style -> Html Msg
viewAscentTripIndicator maybeTrip tripDict =
    let
        color =
            Maybe.andThen (\trip -> Dict.get trip.id tripDict) maybeTrip |> Maybe.withDefault Tw.bg_white
    in
    H.div [ A.css [ color, Tw.pl_10 ], E.onClick (Message.OpenTripOverview maybeTrip) ] []
