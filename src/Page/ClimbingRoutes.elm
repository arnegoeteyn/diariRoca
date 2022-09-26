module Page.ClimbingRoutes exposing (Model, Msg, init, update, view)

import Data
import DataAccessors as MA
import DataUtilities
import Dict
import Form.Criterium exposing (selectionCriterium, selectionWithSearchCriterium, textCriterium)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Message exposing (ClimbingRoutesPageMsg(..))
import Modal
import Select
import Session
import Skeleton
import Tailwind.Utilities as Tw
import Utilities
import View.Button as Button
import View.Link as Link



-- Model


type alias ModelContent =
    { selectedClimbingRoute : Maybe Data.ClimbingRoute
    , routeFilter : String
    , routeKindFilter : Maybe Data.ClimbingRouteKind
    , selected : List Data.Sector
    , selectState : Select.State
    , modal : ModalContent
    }


type alias Model =
    Session.ModelEncapsulated ModelContent


type ModalContent
    = Empty
    | ClimbingRouteFormModal
    | DeleteClimbingRouteRequestModal Data.ClimbingRoute



-- INIT


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { routeFilter = ""
      , routeKindFilter = Nothing
      , selected = []
      , selectState = Select.init "sectors"
      , selectedClimbingRoute = Nothing
      , modal = Empty
      , session = session
      }
    , Cmd.none
    )


sectorSelectConfig : Model -> Select.Config Msg Data.Sector
sectorSelectConfig model =
    let
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = \sector -> sector.name ++ " [" ++ MA.getAreaNameSafe model.session.data sector.areaId ++ "]"
            , onSelect = SelectSector
            , toMsg = SelectMsg
            }
    in
    Select.newConfig r
        |> Select.withMultiSelection True
        |> Select.withOnRemoveItem OnRemoveSectorSelection
        |> Select.withPrompt "Filter by sector"



-- UPDATE


type Msg
    = NoOp
    | CloseModal
    | SetRouteFilter String
    | SetClimbingRouteKindFilter (Maybe Data.ClimbingRouteKind)
    | SelectMsg (Select.Msg Data.Sector)
    | SelectSector (Maybe Data.Sector)
    | OnRemoveSectorSelection Data.Sector


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        removeFromSelected item =
            List.filter (\c -> c /= item)
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Empty }, Cmd.none )

        SetRouteFilter filter ->
            ( { model | routeFilter = filter }, Cmd.none )

        SetClimbingRouteKindFilter filter ->
            ( { model | routeKindFilter = filter }, Cmd.none )

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (sectorSelectConfig model) subMsg model.selectState
            in
            ( { model | selectState = updated }, cmd )

        SelectSector maybeSector ->
            ( { model | selected = newSelected maybeSector model.selected }, Cmd.none )

        OnRemoveSectorSelection sector ->
            ( { model | selected = removeFromSelected sector model.selected }, Cmd.none )


newSelected : Maybe a -> List a -> List a
newSelected maybeItem default =
    case maybeItem of
        Nothing ->
            []

        Just item ->
            Utilities.addIfNotPresent item default



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    { title = "Climbing Route"
    , warning = Skeleton.NoProblems
    , session = model.session
    , kids =
        let
            modal =
                Modal.view CloseModal NoOp
        in
        [ H.div [ A.css [ Tw.px_8 ] ]
            [ viewTableHeader model
            , viewRoutesTable model
            , case model.modal of
                _ ->
                    H.text ""

            -- ClimbingRouteFormModal ->
            --     modal
            --         [-- viewClimbingRouteFormModal model
            --         ]
            ]
        ]
    }


viewTableHeader : Model -> Html Msg
viewTableHeader model =
    let
        routes =
            sortedAndFilteredRoutes model

        onRouteFilter =
            Form.Criterium.textCriterium
                "route"
                .routeFilter
                identity
                SetRouteFilter
                model

        onSectorFilter =
            selectionWithSearchCriterium "Sector"
                (sectorSelectConfig model)
                (\aModel -> ( aModel.selected, aModel.selectState ))
                (Dict.toList model.session.data.sectors |> List.map Tuple.second)
                model

        onKindFilter =
            selectionCriterium "Kind"
                (\_ -> "" :: List.map Data.climbingRouteKindToString Data.climbingRouteKindEnum)
                Data.climbingRouteKindFromString
                SetClimbingRouteKindFilter
                ""
                Nothing
    in
    H.div [ A.css [ Tw.flex, Tw.justify_between ] ]
        [ H.div [ A.css [ Tw.py_4 ] ]
            [ H.h2 [ A.css [] ]
                [ H.text <| Utilities.stringFromList [ (String.fromInt << List.length) routes, " routes " ]

                -- , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenClimbingRouteForm Nothing))
                ]
            , H.div [ A.css [] ]
                [ onRouteFilter
                , onSectorFilter
                , onKindFilter
                ]
            ]
        ]


viewRoutesTable : Model -> Html Msg
viewRoutesTable model =
    H.div
        [ A.css
            [ Tw.overflow_x_auto
            , Tw.relative
            , Tw.shadow_md
            , Tw.border
            , Tw.border_solid
            , Tw.rounded_md
            ]
        , A.id "route-container"
        ]
        [ H.table
            [ A.css [ Tw.w_full, Tw.text_sm ] ]
            [ viewRoutesTableHeader model
            , viewRoutesTableBody model
            ]
        ]


viewRoutesTableHeader : Model -> Html Msg
viewRoutesTableHeader model =
    H.thead
        [ A.css
            [ Tw.text_xs
            , Tw.text_gray_700
            , Tw.uppercase
            , Tw.bg_gray_50
            ]
        ]
        [ H.tr []
            (List.map
                (\header ->
                    H.th
                        [ A.scope "col"
                        , A.css
                            [ Tw.py_3
                            , Tw.px_6
                            ]
                        ]
                        [ H.text header ]
                )
                [ "Grade", "Name", "Kind", "Ascents", "Actions" ]
            )
        ]


viewRoutesTableBody : Model -> Html Msg
viewRoutesTableBody model =
    H.tbody []
        (List.map
            (\route ->
                viewRouteRow model route
             --  , viewRouteDetail model route
            )
            (sortedAndFilteredRoutes model)
        )


viewRouteRow : Model -> Data.ClimbingRoute -> Html Msg
viewRouteRow model route =
    let
        sector =
            MA.getSector model.session.data route.sectorId

        -- sectorLink =
        --     Link.buttonLink (MA.getSectorAndAreaNameSafe model route.sectorId) (ClimbingRoutesPageMessage <| SelectSector sector)
        ascents =
            MA.getAscents model.session.data route

        cellCss =
            A.css
                [ Tw.py_4
                , Tw.px_6
                ]
    in
    H.tr
        [ A.css [ Tw.bg_white, Tw.border_b ]
        , A.scope "row"
        , A.id <| "route-" ++ String.fromInt route.id
        ]
        [ H.td [ cellCss ] [ H.text route.grade ]
        , H.td [ cellCss, A.css [ Tw.text_left ] ]
            [ H.div [ A.css [ Tw.font_bold ] ] [ H.text route.name ]

            -- , sectorLink
            ]
        , H.td [ cellCss ] [ H.text (Data.climbingRouteKindToString route.kind) ]
        , H.td [ cellCss ] [ (H.text << String.fromInt << List.length) ascents ]
        , H.td []
            [ Button.deleteButton
                (Button.defaultOptions
                    -- |> Button.withMsg (Message.DeleteClimbingRouteRequested route)
                    |> Button.withKind Button.Icon
                )
            , Button.editButton
                (Button.defaultOptions
                    -- |> Button.withMsg (Message.OpenClimbingRouteForm (Just route))
                    |> Button.withKind Button.Icon
                )
            , Button.gotoButton
                (Button.defaultOptions
                    |> Button.withHref ("routes/" ++ String.fromInt route.id)
                    |> Button.withKind Button.Icon
                )
            ]
        ]



--| Utilities


sortedAndFilteredRoutes : Model -> List Data.ClimbingRoute
sortedAndFilteredRoutes model =
    let
        routes =
            Dict.toList model.session.data.climbingRoutes |> List.map Tuple.second
    in
    (DataUtilities.filterRoutes model.routeFilter model.selected model.routeKindFilter
        >> DataUtilities.sortRoutes
    )
        routes
