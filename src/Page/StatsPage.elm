module Page.StatsPage exposing (GradeCountDict, PerGradeStat, view)

import Data exposing (ClimbingRouteKind(..))
import Dict exposing (Dict)
import Html.Styled as H
import Html.Styled.Attributes as A
import Message exposing (Msg)
import Model exposing (Model)
import ModelAccessors as MA
import Set
import Tailwind.Utilities as Tw


view : Model -> H.Html Msg
view model =
    let
        countsSport =
            countPerGrade model Sport

        countsBoulder =
            countPerGrade model Boulder
    in
    H.div []
        [ H.text "Stats"
        , H.div [ A.css [ Tw.grid, Tw.grid_cols_2 ] ] [ viewStatsTable countsSport, viewStatsTable countsBoulder ]
        ]


viewStatsTable : PerGradeStat -> H.Html Msg
viewStatsTable ( kind, stats ) =
    let
        count =
            Dict.foldl (\_ -> (+)) 0 stats

        title =
            case kind of
                Sport ->
                    "Sport Routes: " ++ String.fromInt count

                Boulder ->
                    "Boulders: " ++ String.fromInt count
    in
    H.div []
        [ H.h2 []
            [ H.text title ]
        , H.table
            []
          <|
            List.map
                (\stat ->
                    H.tr []
                        [ H.td [] [ H.text <| Tuple.first stat ]
                        , H.td [] [ H.text <| String.fromInt <| Tuple.second stat ]
                        ]
                )
                (List.sortBy Tuple.first (Dict.toList stats))
        ]



--| Utilities


type alias GradeCountDict =
    Dict String Int


type alias PerGradeStat =
    ( ClimbingRouteKind, GradeCountDict )


countPerGrade : Model -> ClimbingRouteKind -> PerGradeStat
countPerGrade model kind =
    ( kind
    , Dict.foldl (\_ v a -> Set.insert v.routeId a)
        Set.empty
        model.ascents
        |> Set.foldl
            (\k a ->
                let
                    route =
                        MA.getClimbingRoute model k
                in
                case route of
                    Just r ->
                        if r.kind == kind then
                            Dict.insert r.grade
                                (Maybe.withDefault 0 (Dict.get r.grade a) |> (+) 1)
                                a

                        else
                            a

                    Nothing ->
                        a
            )
            Dict.empty
    )
