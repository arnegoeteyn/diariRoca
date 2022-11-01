module View.PerGradeStat exposing (countPerGrade, viewChart, viewHeader, viewTable)

import Axis
import Data exposing (ClimbingRouteKind(..), Data)
import DataAccessors as MA
import Dict
import Html.Styled as H
import Model exposing (Model)
import Scale exposing (BandScale, ContinuousScale, bandwidth, defaultBandConfig)
import Set
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (alignmentBaseline, class, fontSize, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Transform(..), px)


viewHeader : PerGradeStat -> H.Html msg
viewHeader ( kind, stats ) =
    let
        count =
            List.foldl (Tuple.second >> (+)) 0 stats
    in
    H.h2 []
        [ H.text <|
            case kind of
                Sport ->
                    "Sport Routes: " ++ String.fromInt count

                Boulder ->
                    "Boulders: " ++ String.fromInt count
        ]


viewTable : PerGradeStat -> H.Html msg
viewTable ( kind, stats ) =
    H.table
        []
    <|
        List.map
            (\stat ->
                H.tr []
                    [ H.td [] [ H.text <| Tuple.first stat ]
                    , H.td [] [ H.text <| String.fromInt <| Tuple.second stat ]
                    ]
            )
            (List.sortBy Tuple.first stats)


w : Float
w =
    450


h : Float
h =
    450


padding : Float
padding =
    30


xScale : GradeCount -> ContinuousScale Float
xScale grades =
    Scale.linear
        ( 0, w - 2 * padding )
        ( 0, Maybe.map (toFloat >> (*) 1.25) (List.maximum (List.map Tuple.second grades)) |> Maybe.withDefault 5 )


yScale : GradeCount -> BandScale String
yScale grades =
    List.map Tuple.first grades
        |> List.reverse
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, h - 2 * padding )


xAxis : GradeCount -> Svg msg
xAxis grades =
    Axis.bottom [ Axis.tickCount 5 ] (xScale grades)


yAxis : GradeCount -> Svg msg
yAxis grades =
    Axis.left [] (Scale.toRenderable identity (yScale grades))


column : ContinuousScale Float -> BandScale String -> ( String, Int ) -> Svg msg
column calcXScale calcYScale ( grade, count ) =
    let
        fCount =
            toFloat count

        barSize =
            bandwidth calcYScale
    in
    g [ class [ "column" ] ]
        [ rect
            [ x <| 1
            , y <| Scale.convert calcYScale grade
            , width <| Scale.convert calcXScale fCount
            , height <| Scale.bandwidth calcYScale
            ]
            []
        , text_
            [ x <| Scale.convert calcXScale fCount + 10
            , y <| Scale.convert calcYScale grade + (barSize / 2)
            , fontSize (px 10)
            , alignmentBaseline AlignmentCentral
            ]
            [ text <| String.fromInt count ]
        ]


view : PerGradeStat -> Svg msg
view ( _, stats ) =
    svg [ viewBox 0 0 w h ]
        [ style [] [ text """
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis stats ]
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis stats ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (xScale stats) (yScale stats)) stats
        ]


viewChart : PerGradeStat -> H.Html msg
viewChart stats =
    H.fromUnstyled <| view stats



--| Utilities


type alias GradeCount =
    List ( String, Int )


type alias PerGradeStat =
    ( ClimbingRouteKind, GradeCount )


countPerGrade : Data -> ClimbingRouteKind -> PerGradeStat
countPerGrade data kind =
    ( kind
    , Dict.toList
        (Dict.foldl (\_ v a -> Set.insert v.routeId a)
            Set.empty
            data.ascents
            |> Set.foldl
                (\k a ->
                    let
                        route =
                            MA.getClimbingRoute data k
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
    )
