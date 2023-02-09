module Page.Sector exposing (..)

import Component.ClimbingRouteList as ClimbingRouteList
import Data exposing (Area, ClimbingRouteKind, Data, Sector)
import DataAccessors as DA exposing (getRoutesFromSector)
import DataUtilities as DU
import Dict
import Form.Criterium exposing (formSelectionWithSearchCriterium, formTextCriterium, updateSelectCriteriumMsg)
import Form.Form as Form
import Form.Forms exposing (SelectionCriterium, idForForm, updateName, validateNonEmpty, viewErrors)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Modal
import Select
import Session
import Skeleton
import Tailwind.Utilities as TW
import Utilities exposing (notFound, orElse)
import View.Button as Button



-- Model


type alias ModelContent =
    { sectorId : Int
    }


type alias Model =
    Session.ModelEncapsulated ModelContent



-- Init


init : Session.Model -> Int -> ( Model, Cmd Msg )
init session sectorId =
    ( { session = session
      , sectorId = sectorId
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



--View


view : Model -> Skeleton.Details Msg
view model =
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
                        ]
                    ]
                , ClimbingRouteList.viewRoutes (routeItems model sector)
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


routeItems : Model -> Sector -> ClimbingRouteList.Model
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
