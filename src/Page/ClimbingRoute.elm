module Page.ClimbingRoute exposing (Model, Msg(..), init, update, view)

import Data exposing (ClimbingRoute, Data)
import Forms.Criterium exposing (textCriterium)
import General
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import ModelAccessors as MA
import Skeleton
import Tailwind.Utilities as Tw
import Utilities
import View.Button as Button



-- Model


type alias Model =
    { mediaLink : String
    , mediaLabel : String
    , routeId : Int
    }


init : Int -> ( Model, Cmd Msg )
init id =
    ( { mediaLink = ""
      , mediaLabel = ""
      , routeId = id
      }
    , Cmd.none
    )



-- Update


type Msg
    = SetMediaLink String
    | SetMediaLabel String
      -- General
    | DeleteClimbingRouteRequested ClimbingRoute


update : Msg -> Model -> ( Model, Cmd Msg, General.Msg )
update msg model =
    case msg of
        SetMediaLink link ->
            ( { model | mediaLink = link }, Cmd.none, General.None )

        SetMediaLabel label ->
            ( { model | mediaLabel = label }, Cmd.none, General.None )

        DeleteClimbingRouteRequested climbingRoute ->
            ( model, Cmd.none, General.DeleteClimbingRouteRequested climbingRoute )



-- View


view : Model -> Data -> Skeleton.Details Msg
view model data =
    let
        maybeRoute =
            MA.getClimbingRoute data model.routeId
    in
    { title = "Climbing Route"
    , header = []
    , warning = Skeleton.NoProblems
    , kids =
        [ case maybeRoute of
            Just route ->
                H.div []
                    [ H.h1 [] [ H.text route.name ]
                    , viewRouteDetail model route
                    ]

            Nothing ->
                H.text "route not found"
        ]
    }


viewRouteDetail : Model -> ClimbingRoute -> Html Msg
viewRouteDetail model route =
    H.div [ A.css [ Tw.grid, Tw.gap_4, Tw.grid_cols_3 ] ]
        [ H.div [ A.css [ Tw.flex, Tw.flex_col, Tw.justify_around, Tw.col_span_2 ] ]
            [ viewRouteInfo model route
            , case route.beta of
                Just beta ->
                    H.details []
                        [ H.summary [] [ H.text "Beta" ]
                        , H.text beta
                        ]

                Nothing ->
                    H.text ""

            -- , viewAscentsList model route
            ]
        , H.div [ A.css [] ]
            [ viewRouteImage route
            , viewRouteMedia model route
            ]
        , H.div []
            [ Button.deleteButton (Button.defaultOptions |> Button.withMsg (DeleteClimbingRouteRequested route) |> Button.withKind Button.TextAndIcon)

            -- , Button.editButton (Button.defaultOptions |> Button.withMsg (General.OpenClimbingRouteForm (Just route)) |> Button.withKind Button.TextAndIcon)
            ]
        ]


viewRouteInfo : Model -> ClimbingRoute -> Html Msg
viewRouteInfo _ climbingRoute =
    H.div [ A.css [] ]
        [ H.text <| Maybe.withDefault "" climbingRoute.comment
        ]


viewRouteImage : ClimbingRoute -> Html Msg
viewRouteImage _ =
    H.img [ A.src "https://www.barcelona-tourist-guide.com/images/ext/attractions/montserrat/L550/montserrat-barcelona-29.jpg", A.css [ Tw.col_auto, Tw.mx_auto ] ]
        []


viewRouteMedia : Model -> ClimbingRoute -> Html Msg
viewRouteMedia model route =
    let
        hasMedia =
            (not << List.isEmpty) route.media

        addMediaInput =
            H.div []
                [ textCriterium "Link" .mediaLink identity SetMediaLink model
                , textCriterium "Link" .mediaLabel identity SetMediaLabel model

                -- , Button.addButton (Button.defaultOptions |> Button.withMsg (AddMediaToRoute route))
                ]
    in
    H.div []
        [ H.div [] [ H.text <| Utilities.stringFromList [ String.fromInt <| List.length route.media, " media:" ] ]
        , if hasMedia then
            H.ul [] <|
                List.map
                    (\media ->
                        H.li []
                            [ H.a [ A.css [ Tw.break_words ], A.href media.link, A.target "_blank" ] [ H.text media.label ]

                            -- , H.button [ E.onClick <| RemoveMedia route media ] [ H.text "x" ]
                            ]
                    )
                    route.media

          else
            H.text ""
        , addMediaInput
        ]



-- viewAscentsList : Model -> ClimbingRoute -> Html Msg
-- viewAscentsList model route =
--     let
--         ascents =
--             Ma.getAscents model route
--     in
--     H.div [ A.css [] ]
--         [ H.h3 [ A.css [] ]
--             [ H.text (Utilities.stringFromList [ String.fromInt <| List.length ascents, " ascents:" ])
--             , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenAscentForm Nothing route))
--             ]
--         , H.div [ A.css [ Tw.divide_solid, Tw.divide_y_2, Tw.divide_x_0 ] ] <|
--             List.map
--                 (\ascent ->
--                     H.div [ A.css [ Tw.p_1_dot_5 ] ]
--                         [ H.div [ A.css [ Tw.grid, Tw.grid_cols_3 ] ]
--                             [ H.div [ A.css [] ] [ H.text <| Date.toIsoString ascent.date ]
--                             , H.div [ A.css [] ] [ H.text (ascentKindToString ascent.kind) ]
--                             , H.div
--                                 []
--                                 [ Button.deleteButton (Button.defaultOptions |> Button.withMsg (Message.DeleteAscentRequested ascent))
--                                 , Button.editButton (Button.defaultOptions |> Button.withMsg (Message.OpenAscentForm (Just ascent) route))
--                                 ]
--                             ]
--                         , H.div [ A.css [] ] [ H.text <| Maybe.withDefault "" ascent.comment ]
--                         ]
--                 )
--                 ascents
--         ]
