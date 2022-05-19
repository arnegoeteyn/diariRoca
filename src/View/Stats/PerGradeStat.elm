module View.Stats.PerGradeStat exposing (countPerGrade, viewChart, viewTable)

import Axis
import Data exposing (ClimbingRouteKind(..))
import Dict exposing (Dict)
import Html.Styled as H
import Html.Styled.Attributes as A
import Message exposing (Msg)
import Model exposing (Model)
import ModelAccessors as MA
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Set
import Tailwind.Utilities as Tw
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))


viewTable : PerGradeStat -> H.Html Msg
viewTable ( kind, stats ) =
    let
        count =
            List.foldl (Tuple.second >> (+)) 0 stats

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
                (List.sortBy Tuple.first stats)
        ]


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : GradeCount -> BandScale String
xScale stats =
    List.map Tuple.first stats
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : GradeCount -> ContinuousScale Float
yScale grades =
    Scale.linear
        ( h - 2 * padding, 0 )
        ( 0, Maybe.map (toFloat >> (*) 1.25) (List.maximum (List.map Tuple.second grades)) |> Maybe.withDefault 5 )


xAxis : GradeCount -> Svg msg
xAxis stats =
    Axis.bottom [] (Scale.toRenderable identity (xScale stats))


yAxis : GradeCount -> Svg msg
yAxis grades =
    Axis.left [ Axis.tickCount 5 ] (yScale grades)


column : BandScale String -> ContinuousScale Float -> ( String, Int ) -> Svg msg
column calcXScale calcYScale ( grade, count ) =
    let
        fCount =
            toFloat count
    in
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert calcXScale grade
            , y <| Scale.convert calcYScale fCount
            , width <| Scale.bandwidth calcXScale
            , height <| h - Scale.convert calcYScale fCount - 2 * padding
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable identity calcXScale) grade
            , y <| Scale.convert calcYScale fCount - 5
            , textAnchor AnchorMiddle
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
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis stats ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis stats ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (xScale stats) (yScale stats)) stats
        ]


viewChart : PerGradeStat -> H.Html Msg
viewChart stats =
    H.fromUnstyled <| view stats



--| Utilities


type alias GradeCount =
    List ( String, Int )


type alias PerGradeStat =
    ( ClimbingRouteKind, GradeCount )


countPerGrade : Model -> ClimbingRouteKind -> PerGradeStat
countPerGrade model kind =
    ( kind
    , Dict.toList
        (Dict.foldl (\_ v a -> Set.insert v.routeId a)
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
    )
