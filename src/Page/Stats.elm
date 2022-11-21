module Page.Stats exposing (view)

import Data exposing (ClimbingRouteKind(..))
import Html.Styled as H
import Html.Styled.Attributes as A
import Session
import Skeleton
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import View.PerGradeStat as PGS


view : Session.Model -> Skeleton.Details msg
view session =
    { title = "Stats"
    , warning = Skeleton.NoProblems
    , session = session
    , kids =
        let
            countsSport =
                PGS.countPerGrade session.data Sport

            countsBoulder =
                PGS.countPerGrade session.data Boulder

            statSet s =
                List.map (\f -> f s) [ PGS.viewHeader, PGS.viewChart ]
        in
        [ H.div []
            [ H.text "Stats"
            , H.div [ A.css [ B.lg [ Tw.grid, Tw.grid_cols_2 ] ] ]
                [ H.div [] <| statSet countsSport
                , H.div [] <| statSet countsBoulder
                ]
            ]
        ]
    }
