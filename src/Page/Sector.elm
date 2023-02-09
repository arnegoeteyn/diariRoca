module Page.Sector exposing (..)

import Component.ClimbingRouteList as ClimbingRouteList
import Data exposing (Area, ClimbingRouteKind, Data, Sector)
import DataAccessors as DA exposing (getRoutesFromSector)
import DataUtilities as DU
import Dict
import Form.Criterium exposing (formSelectionWithSearchCriterium, formTextCriterium, updateSelectCriteriumMsg)
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
    { sectorId : Int
    , modal : ModalContent
    , climbingRouteForm : ( ClimbingRouteForm.ClimbingRouteForm, Maybe Data.ClimbingRoute )
    }


type alias Model =
    Session.ModelEncapsulated ModelContent


type ModalContent
    = Empty
    | DeleteClimbingRouteRequestModal Data.ClimbingRoute
    | ClimbingRouteFormModal



-- Init


init : Session.Model -> Int -> ( Model, Cmd Msg )
init session sectorId =
    ( { session = session
      , sectorId = sectorId
      , modal = Empty
      , climbingRouteForm = ( ClimbingRouteForm.initClimbingRouteForm ClimbingRouteForm.emptyValues, Nothing )
      }
    , Cmd.none
    )


climbingRouteFormSettings : ClimbingRouteForm.ClimbingRouteFormSettings Msg
climbingRouteFormSettings =
    { onSave = NoOp
    , onUpdate = \_ -> NoOp
    , onSelect = \_ -> NoOp
    , selectToMsg = \_ -> NoOp
    }



-- Update


type Msg
    = NoOp
    | CloseModal
    | DeleteClimbingRouteRequested Data.ClimbingRoute
    | DeleteClimbingRouteConfirmation Data.ClimbingRoute
    | OpenClimbingRouteForm (Maybe Data.ClimbingRoute)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Empty }, Cmd.none )

        DeleteClimbingRouteRequested climbingRoute ->
            ( { model | modal = DeleteClimbingRouteRequestModal climbingRoute }
            , Cmd.none
            )

        DeleteClimbingRouteConfirmation climbingRoute ->
            Session.deleteClimbingRoute climbingRoute model.session
                |> Session.assign { model | modal = Empty }

        OpenClimbingRouteForm maybeClimbingRoute ->
            let
                s =
                    -- todo shouldnt be her
                    ( DA.getSector model.session.data model.sectorId
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                    , Select.init "climbingRouteFormSectorId"
                    )
                kind = 
                    Maybe.withDefault "sport" (mostOccuringKind model) 

                values =
                    ClimbingRouteForm.valuesFromMaybeRoute model.session maybeClimbingRoute
            in
            ( { model
                | modal = ClimbingRouteFormModal
                , climbingRouteForm =
                    ( ClimbingRouteForm.initClimbingRouteForm { values 
                        | sectorId = s
                        , kind = kind}
                    , maybeClimbingRoute
                    )
              }
            , Cmd.none
            )



--View


view : Model -> Skeleton.Details Msg
view model =
    let
        modal =
            Modal.view CloseModal NoOp
    in
    { title = "Sector"
    , warning = Skeleton.NoProblems
    , session = model.session
    , kids =
        notFound
            (\sector ->
                [ H.div []
                    [ H.p []
                        [ H.text sector.name
                        , H.text " "
                        , mostOccuringKindText model
                        , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenClimbingRouteForm Nothing))
                        ]
                    ]
                , ClimbingRouteList.viewRoutes (routeItems model sector)
                , case model.modal of
                    Empty ->
                        H.text ""

                    DeleteClimbingRouteRequestModal climbingRoute ->
                        modal (DeleteClimbingRouteModal.view climbingRoute DeleteClimbingRouteConfirmation)

                    ClimbingRouteFormModal ->
                        modal
                            (H.div []
                                [ H.h2 [] [ H.text "New climbingroute" ], ClimbingRouteForm.viewClimbingRouteForm climbingRouteFormSettings model ]
                            )
                ]
            )
            (DA.getSector model.session.data model.sectorId)
    }


mostOccuringKindText : Model -> H.Html Msg
mostOccuringKindText m =
    case mostOccuringKind m of
        Just x ->
            H.span [ A.css [ TW.text_gray_500 ] ] [ H.text <| "(Mostly " ++ x ++ ")" ]

        Nothing ->
            H.text ""



-- Utilities


routeItems : Model -> Sector -> ClimbingRouteList.Props Msg
routeItems model sector =
    let
        climbingRoutes =
            DA.getRoutesFromSector model.sectorId model.session.data
    in
    List.map
        (\route ->
            { route = route
            , sector = sector
            , ascents = DA.getAscents model.session.data route
            , deleteClimbingRouteMsg = DeleteClimbingRouteRequested
            }
        )
        climbingRoutes


mostOccuringKind : Model -> Maybe String
mostOccuringKind model =
    let
        routes =
            DA.getRoutesFromSector model.sectorId model.session.data
    in
    List.map (.kind >> Data.climbingRouteKindToString) routes
        |> Utilities.mostOccuring
