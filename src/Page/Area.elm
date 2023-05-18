module Page.Area exposing (..)

import Component.ClimbingRouteList as ClimbingRouteList
import Data exposing (Area, ClimbingRouteKind, Data, Sector)
import DataAccessors as DA exposing (getRoutesFromSector)
import DataUtilities as DU
import Dict
import Form.Criterium as Criterium exposing (formSelectionCriterium, formSelectionWithSearchCriterium, formTextAreaCriterium, formTextCriterium, textCriterium, updateSelectCriteriumMsg)
import Form.Form as Form
import Form.Forms exposing (SelectionCriterium, idForForm, updateName, validateNonEmpty, viewErrors)
import Form.Forms.ClimbingRouteForm as ClimbingRouteForm
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Modal
import Page.ClimbingRoute exposing (Msg(..))
import Select
import Session
import Skeleton
import Tailwind.Utilities as TW
import Utilities exposing (notFound, orElse)
import View.Button as Button
import View.Modal.DeleteClimbingRoute as DeleteClimbingRouteModal



-- Model


type alias ModelContent =
    { areaId : Int
    , modal : ModalContent
    , climbingRoutesFilter : ClimbingRouteList.ClimbingRoutesFilter

    -- , climbingRouteForm : ( ClimbingRouteForm.ClimbingRouteForm, Maybe Data.ClimbingRoute )
    }


type alias Model =
    Session.ModelEncapsulated ModelContent


type ModalContent
    = Empty



-- | DeleteClimbingRouteRequestModal Data.ClimbingRoute
-- | ClimbingRouteFormModal
-- Init


init : Session.Model -> Int -> ( Model, Cmd Msg )
init session areaId =
    ( { session = session
      , areaId = areaId
      , modal = Empty
      , climbingRoutesFilter = ClimbingRouteList.initClimbingRoutesFilter

      -- , climbingRouteForm = ( ClimbingRouteForm.initClimbingRouteForm ClimbingRouteForm.emptyValues, Nothing )
      }
    , Cmd.none
    )



-- climbingRouteFormSettings : ClimbingRouteForm.ClimbingRouteFormSettings Msg
-- climbingRouteFormSettings =
--     { onSave = SaveClimbingRouteForm
--     , onUpdate = UpdateClimbingRouteForm
--     , onSelect = ClimbingRouteFormSelectSector
--     , selectToMsg = ClimbingRouteFormSelectSectorMsg
--     }
-- Update


type Msg
    = NoOp
    | CloseModal
    | ClimbingRoutesFilterUpdated ClimbingRouteList.ClimbingRoutesFilter



-- | DeleteClimbingRouteRequested Data.ClimbingRoute
-- | DeleteClimbingRouteConfirmation Data.ClimbingRoute
-- | OpenClimbingRouteForm (Maybe Data.ClimbingRoute)
-- -- form
-- | SaveClimbingRouteForm
-- | UpdateClimbingRouteForm ClimbingRouteForm.ClimbingRouteForm
-- | ClimbingRouteFormSelectSector (Maybe Sector)
-- | ClimbingRouteFormSelectSectorMsg (Select.Msg Sector)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Empty }, Cmd.none )

        ClimbingRoutesFilterUpdated filter ->
            ( { model | climbingRoutesFilter = filter }, Cmd.none )



-- DeleteClimbingRouteRequested climbingRoute ->
--     ( { model | modal = DeleteClimbingRouteRequestModal climbingRoute }
--     , Cmd.none
--     )
--
-- DeleteClimbingRouteConfirmation climbingRoute ->
--     Session.deleteClimbingRoute climbingRoute model.session
--         |> Session.assign { model | modal = Empty }
--
-- OpenClimbingRouteForm maybeClimbingRoute ->
--     let
--         s =
--             -- todo shouldnt be her
--             ( DA.getSector model.session.data model.sectorId
--                 |> Maybe.map List.singleton
--                 |> Maybe.withDefault []
--             , Select.init "climbingRouteFormSectorId"
--             )
--         kind =
--             Maybe.withDefault "sport" (mostOccuringKind model)
--
--         values =
--             ClimbingRouteForm.valuesFromMaybeRoute model.session maybeClimbingRoute
--     in
--     ( { model
--         | modal = ClimbingRouteFormModal
--         , climbingRouteForm =
--             ( ClimbingRouteForm.initClimbingRouteForm { values
--                 | sectorId = s
--                 , kind = kind}
--             , maybeClimbingRoute
--             )
--       }
--     , Cmd.none
--     )
--
-- -- Climbing route form
-- UpdateClimbingRouteForm values ->
--     ( { model | climbingRouteForm = Utilities.replaceFirst values model.climbingRouteForm }, Cmd.none )
--
-- ClimbingRouteFormSelectSector maybeSector ->
--     let
--         newForm f =
--             { f
--                 | sectorId =
--                     Tuple.mapFirst
--                         (\_ -> Maybe.withDefault [] <| Maybe.map List.singleton maybeSector)
--                         f.sectorId
--             }
--     in
--     ( { model | climbingRouteForm = Tuple.mapFirst (Form.mapValues newForm) model.climbingRouteForm }, Cmd.none )
--
-- ClimbingRouteFormSelectSectorMsg subMsg ->
--     let
--         ( updatedForm, cmd ) =
--             Criterium.updateSelectCriteriumMsg .sectorId
--                 (\selected values -> { values | sectorId = selected })
--                 (ClimbingRouteForm.climbingRouteFormSectorSelectConfig climbingRouteFormSettings model.session)
--                 subMsg
--                 (Tuple.first model.climbingRouteForm)
--     in
--     ( { model
--         | climbingRouteForm =
--             Utilities.replaceFirst updatedForm model.climbingRouteForm
--       }
--     , cmd
--     )
--
-- SaveClimbingRouteForm ->
--     let
--         ( newForm, maybeClimbingRoute ) =
--             ClimbingRouteForm.validateClimbingRouteForm model
--
--         updatedModel =
--             { model
--                 | climbingRouteForm = Utilities.replaceFirst newForm model.climbingRouteForm
--             }
--     in
--     case maybeClimbingRoute of
--         Just climbingRoute ->
--             Session.addClimbingRoute climbingRoute model.session
--                 |> Session.assign { updatedModel | modal = Empty }
--
--         Nothing ->
--             ( updatedModel, Cmd.none )
--View


view : Model -> Skeleton.Details Msg
view model =
    let
        modal =
            Modal.view CloseModal NoOp
    in
    { title = "Area"
    , warning = Skeleton.NoProblems
    , session = model.session
    , kids =
        notFound
            (\area ->
                [ H.div []
                    [ H.p []
                        [ H.text area.name
                        , H.text " "

                        -- , mostOccuringKindText model
                        -- , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenClimbingRouteForm Nothing))
                        ]
                    ]
                , ClimbingRouteList.viewClimbingRoutesFilter { onUpdate = ClimbingRoutesFilterUpdated } model.climbingRoutesFilter
                , ClimbingRouteList.viewRoutes (routeItems model area)
                , case model.modal of
                    Empty ->
                        H.text ""

                -- DeleteClimbingRouteRequestModal climbingRoute ->
                --     modal (DeleteClimbingRouteModal.view climbingRoute DeleteClimbingRouteConfirmation)
                --
                -- ClimbingRouteFormModal ->
                --     modal
                --         (H.div []
                --             [ H.h2 [] [ H.text "New climbingroute" ], ClimbingRouteForm.viewClimbingRouteForm climbingRouteFormSettings model ]
                --         )
                ]
            )
            (DA.getArea model.session.data model.areaId)
    }



-- mostOccuringKindText : Model -> H.Html Msg
-- mostOccuringKindText m =
--     case mostOccuringKind m of
--         Just x ->
--             H.span [ A.css [ TW.text_gray_500 ] ] [ H.text <| "(Mostly " ++ x ++ ")" ]
--
--         Nothing ->
--             H.text ""
-- Utilities


routeItems : Model -> Area -> ClimbingRouteList.Props Msg
routeItems model area =
    let
        climbingRoutes =
            DA.getRoutesFromArea area.id model.session.data
    in
    List.map
        (\( route, sector ) ->
            { route = route
            , sector = sector
            , ascents = DA.getAscents model.session.data route
            , deleteClimbingRouteMsg = \_ -> NoOp

            -- , deleteClimbingRouteMsg = DeleteClimbingRouteRequested
            }
        )
        climbingRoutes
        |> ClimbingRouteList.filterClimbingRoutes model.climbingRoutesFilter



--
--
-- mostOccuringKind : Model -> Maybe String
-- mostOccuringKind model =
--     let
--         routes =
--             DA.getRoutesFromSector model.sectorId model.session.data
--     in
--     List.map (.kind >> Data.climbingRouteKindToString) routes
--         |> Utilities.mostOccuring
