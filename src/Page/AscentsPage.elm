module Page.AscentsPage exposing (..)

import Css
import Data exposing (Ascent, Trip)
import Date
import Dict exposing (Dict)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import List exposing (sortBy)
import Message exposing (Msg(..))
import Model exposing (Model)
import ModelAccessors as MA
import Tailwind.Utilities as Tw
import Utilities
import View.Ascent as Ascent exposing (viewAscentTripIndicator)


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.text "ascents"
        , H.div [ A.css [] ] <|
            List.map
                (\ascent ->
                    H.div [ A.css [ Tw.flex, Tw.flex_row ], E.onClick Dummy ]
                        [ viewAscentTripIndicator (MA.getTripFromDate model ascent.date |> Maybe.map .id) (tripColorDict model.trips)
                        , H.div [ A.css [ Tw.flex_grow, Tw.border, Tw.border_solid, Tw.py_4 ] ] [ viewAscentRow model ascent ]
                        ]
                )
                (sortedAndFilteredAscents model)
        ]


viewAscentRow : Model -> Ascent -> H.Html Msg
viewAscentRow model ascent =
    Ascent.viewAscentRow { ascent = ascent, routeName = MA.getClimbingRouteName model ascent.routeId }



--| Utilities


sortedAndFilteredAscents : Model -> List Ascent
sortedAndFilteredAscents model =
    let
        ascents =
            Dict.toList model.ascents
                |> List.map Tuple.second
                |> Utilities.sortByDescending (.date >> Date.toIsoString)
    in
    ascents


tripColorDict : Dict Int Trip -> Dict Int Css.Style
tripColorDict trips =
    let
        availableColors =
            [ Tw.bg_green_400
            , Tw.bg_red_400
            , Tw.bg_blue_400
            , Tw.bg_pink_400
            , Tw.bg_purple_400
            ]

        difference =
            Dict.size trips // List.length availableColors |> (+) 1

        colors =
            List.repeat difference availableColors |> List.concat
    in
    Utilities.dictToList trips
        |> sortBy (.from >> Date.toRataDie)
        |> List.map .id
        |> (\sorted -> List.map2 Tuple.pair sorted colors)
        |> Dict.fromList
