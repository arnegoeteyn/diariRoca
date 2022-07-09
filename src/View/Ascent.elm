module View.Ascent exposing (viewAscentTripIndicator)

import Css
import Data exposing (Ascent, Trip)
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg)
import Tailwind.Utilities as Tw


viewAscentTripIndicator : Maybe Trip -> Dict Int Css.Style -> Html Msg
viewAscentTripIndicator maybeTrip tripDict =
    let
        color =
            Maybe.andThen (\trip -> Dict.get trip.id tripDict) maybeTrip |> Maybe.withDefault Tw.bg_white

        message =
            case maybeTrip of
                Just trip ->
                    E.onClick (Message.OpenTripOverview trip)

                Nothing ->
                    E.onClick (Message.OpenTripForm Nothing)
    in
    H.div [ A.css [ color, Tw.pl_10 ], message ] []
