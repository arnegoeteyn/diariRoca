module Page.StatsPage exposing (view)

import Data exposing (ClimbingRouteKind(..))
import Html.Styled as H
import Html.Styled.Attributes as A
import Message exposing (Msg)
import Model exposing (Model)
import Tailwind.Utilities as Tw
import View.Stats.PerGradeStat as PGS


view : Model -> H.Html Msg
view model =
    let
        countsSport =
            PGS.countPerGrade model Sport

        countsBoulder =
            PGS.countPerGrade model Boulder
    in
    H.div []
        [ H.text "Stats"
        , H.div [ A.css [ Tw.grid, Tw.grid_cols_2 ] ]
            [ PGS.viewTable countsSport, PGS.viewTable countsBoulder, PGS.viewChart countsSport, PGS.viewChart countsBoulder ]
        ]
