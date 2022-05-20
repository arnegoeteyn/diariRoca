module Page.StatsPage exposing (view)

import Data exposing (ClimbingRouteKind(..))
import Html.Styled as H
import Html.Styled.Attributes as A
import Message exposing (Msg)
import Model exposing (Model)
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import View.Stats.PerGradeStat as PGS


view : Model -> H.Html Msg
view model =
    let
        countsSport =
            PGS.countPerGrade model Sport

        countsBoulder =
            PGS.countPerGrade model Boulder

        statSet s =
            List.map (\f -> f s) [ PGS.viewHeader, PGS.viewChart ]
    in
    H.div []
        [ H.text "Stats"
        , H.div [ A.css [ B.lg [ Tw.grid, Tw.grid_cols_2 ] ] ]
            [ H.div [] <| statSet countsSport
            , H.div [] <| statSet countsBoulder
            ]
        ]
