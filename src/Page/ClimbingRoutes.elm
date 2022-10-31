module Page.ClimbingRoutes exposing (Model, Msg, init, update, view)

import Browser.Dom
import Data
import DataAccessors as MA
import DataUtilities
import Dict
import Form.Criterium as Criterium exposing (selectionCriterium, selectionWithSearchCriterium, textCriterium)
import Form.Form as Form
import Form.Forms.ClimbingRouteForm as ClimbingRouteForm
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Modal
import Select
import Session
import Skeleton
import Tailwind.Utilities as Tw
import Task
import Utilities
import View.Button as Button



-- Model


type alias ModelContent =
    { selectedClimbingRoute : Maybe Data.ClimbingRoute
    , routeFilter : String
    , routeKindFilter : Maybe Data.ClimbingRouteKind
    , selected : List Data.Sector
    , selectState : Select.State
    , modal : ModalContent
    , climbingRouteForm : ( ClimbingRouteForm.ClimbingRouteForm, Maybe Data.ClimbingRoute )
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
      , climbingRouteForm = ( ClimbingRouteForm.initClimbingRouteForm Nothing Nothing, Nothing )
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


climbingRouteFormSettings : ClimbingRouteForm.ClimbingRouteFormSettings Msg
climbingRouteFormSettings =
    { onSave = SaveClimbingRouteForm
    , onUpdate = UpdateClimbingRouteForm
    , onSelect = ClimbingRouteFormSelectSector
    , selectToMsg = ClimbingRouteFormSelectSectorMsg
    }



-- UPDATE


type Msg
    = NoOp
    | CloseModal
    | SetRouteFilter String
    | SetClimbingRouteKindFilter (Maybe Data.ClimbingRouteKind)
    | SelectMsg (Select.Msg Data.Sector)
    | SelectSector (Maybe Data.Sector)
    | OnRemoveSectorSelection Data.Sector
    | OpenClimbingRouteForm (Maybe Data.ClimbingRoute)
    | DeleteClimbingRouteRequested Data.ClimbingRoute
    | DeleteClimbingRouteConfirmation Data.ClimbingRoute
      -- ClimbingRouteForm
    | UpdateClimbingRouteForm ClimbingRouteForm.ClimbingRouteForm
    | ClimbingRouteFormSelectSector (Maybe Data.Sector)
    | ClimbingRouteFormSelectSectorMsg (Select.Msg Data.Sector)
    | SaveClimbingRouteForm


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

        OpenClimbingRouteForm maybeClimbingRoute ->
            ( { model
                | modal = ClimbingRouteFormModal
                , climbingRouteForm = ( ClimbingRouteForm.initClimbingRouteForm (Just model.session) maybeClimbingRoute, maybeClimbingRoute )
              }
            , Cmd.none
            )

        DeleteClimbingRouteRequested climbingRoute ->
            ( { model | modal = DeleteClimbingRouteRequestModal climbingRoute }
            , Cmd.none
            )

        DeleteClimbingRouteConfirmation climbingRoute ->
            Session.deleteClimbingRoute climbingRoute { model | modal = Empty }

        -- Climbing route form
        UpdateClimbingRouteForm values ->
            ( { model | climbingRouteForm = Utilities.replaceFirst values model.climbingRouteForm }, Cmd.none )

        ClimbingRouteFormSelectSector maybeSector ->
            let
                newForm f =
                    { f
                        | sectorId =
                            Tuple.mapFirst
                                (\_ -> Maybe.withDefault [] <| Maybe.map List.singleton maybeSector)
                                f.sectorId
                    }
            in
            ( { model | climbingRouteForm = Tuple.mapFirst (Form.mapValues newForm) model.climbingRouteForm }, Cmd.none )

        ClimbingRouteFormSelectSectorMsg subMsg ->
            let
                ( updatedForm, cmd ) =
                    Criterium.updateSelectCriteriumMsg .sectorId
                        (\selected values -> { values | sectorId = selected })
                        (ClimbingRouteForm.climbingRouteFormSectorSelectConfig climbingRouteFormSettings model.session)
                        subMsg
                        (Tuple.first model.climbingRouteForm)
            in
            ( { model
                | climbingRouteForm =
                    Utilities.replaceFirst updatedForm model.climbingRouteForm
              }
            , cmd
            )

        SaveClimbingRouteForm ->
            let
                ( newForm, maybeClimbingRoute ) =
                    ClimbingRouteForm.validateClimbingRouteForm model

                updatedModel =
                    { model
                        | climbingRouteForm = Utilities.replaceFirst newForm model.climbingRouteForm
                    }
            in
            case maybeClimbingRoute of
                Just climbingRoute ->
                    { updatedModel | modal = Empty }
                        |> Session.addClimbingRoute climbingRoute
                        |> Session.assignCommand (showRouteTask climbingRoute)

                Nothing ->
                    ( updatedModel, Cmd.none )


newSelected : Maybe a -> List a -> List a
newSelected maybeItem default =
    case maybeItem of
        Nothing ->
            []

        Just item ->
            Utilities.addIfNotPresent item default


showRouteTask climbingRoute =
    let
        tag =
            "route-" ++ String.fromInt climbingRoute.id
    in
    Task.map (\info -> Browser.Dom.setViewport 0 info.element.y) (Browser.Dom.getElement tag)
        |> Task.andThen identity
        |> Task.attempt (\_ -> NoOp)



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
                Empty ->
                    H.text ""

                ClimbingRouteFormModal ->
                    modal
                        (H.div []
                            [ H.h2 [] [ H.text "New climbingroute" ], ClimbingRouteForm.viewClimbingRouteForm climbingRouteFormSettings model ]
                        )

                DeleteClimbingRouteRequestModal climbingRoute ->
                    modal <| viewDeleteClimbingRouteConfirmation climbingRoute
            ]
        ]
    }


viewDeleteClimbingRouteConfirmation : Data.ClimbingRoute -> Html Msg
viewDeleteClimbingRouteConfirmation route =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete \"", route.name, "\" " ] ]
        , H.button [ E.onClick <| DeleteClimbingRouteConfirmation route ] [ H.text "confirm" ]
        ]


viewTableHeader : Model -> Html Msg
viewTableHeader model =
    let
        routes =
            sortedAndFilteredRoutes model

        onRouteFilter =
            Criterium.textCriterium
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
                , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenClimbingRouteForm Nothing))
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
             -- , viewRouteDetail model route
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
                    |> Button.withMsg (DeleteClimbingRouteRequested route)
                    |> Button.withKind Button.Icon
                )
            , Button.editButton
                (Button.defaultOptions
                    |> Button.withMsg (OpenClimbingRouteForm (Just route))
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
