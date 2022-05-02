module Page.AscentsPage exposing (..)

import Data exposing (Ascent)
import Date
import Dict
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg(..))
import Model exposing (Model)
import ModelAccessors as MA
import Tailwind.Utilities as Tw
import Utilities
import View.Ascent as Ascent


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.text "ascents"
        , H.div [ A.css [] ] <|
            List.map
                (\ascent ->
                    H.div [ A.css [ Tw.border, Tw.border_solid, Tw.py_4 ], E.onClick Dummy ]
                        [ viewAscentRow model ascent ]
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
