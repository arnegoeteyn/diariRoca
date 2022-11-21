module Form.Forms.ClimbingRouteForm exposing (..)

import Data exposing (ClimbingRoute, Sector)
import DataAccessors as DA
import DataUtilities
import Dict
import Form.Criterium as Criterium
import Form.Form as Form exposing (Form(..))
import Form.Forms as Forms
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Select
import Session
import Tailwind.Utilities as TW
import Utilities


type alias Model model =
    { model | session : Session.Model, climbingRouteForm : ( ClimbingRouteForm, Maybe ClimbingRoute ) }


type alias ClimbingRouteForm =
    Form ClimbingRouteFormValues ValidatedClimbingRouteFormValues


type alias ClimbingRouteFormSettings msg =
    { onSave : msg
    , onUpdate : ClimbingRouteForm -> msg
    , onSelect : Maybe Sector -> msg
    , selectToMsg : Select.Msg Sector -> msg
    }


type alias ClimbingRouteFormValues =
    { name : String
    , grade : String
    , comment : String
    , beta : String
    , sectorId : Forms.SelectionCriterium Sector
    , kind : String
    }


type alias ValidatedClimbingRouteFormValues =
    { id : Int
    , name : String
    , grade : String
    , comment : Maybe String
    , beta : Maybe String
    , kind : Data.ClimbingRouteKind
    , sectorId : Int
    }


initClimbingRouteForm : Maybe Session.Model -> Maybe ClimbingRoute -> ClimbingRouteForm
initClimbingRouteForm maybeModel climbingRoute =
    Idle
        { name = Maybe.map .name climbingRoute |> Maybe.withDefault ""
        , grade = Maybe.map .grade climbingRoute |> Maybe.withDefault ""
        , comment = Maybe.andThen .comment climbingRoute |> Maybe.withDefault ""
        , beta = Maybe.andThen .beta climbingRoute |> Maybe.withDefault ""
        , kind = Data.climbingRouteKindToString <| (Maybe.withDefault Data.Sport <| Maybe.map .kind climbingRoute)
        , sectorId =
            ( Maybe.andThen
                (\model -> Maybe.andThen (.sectorId >> DA.getSector model.data) climbingRoute |> Maybe.map List.singleton)
                maybeModel
                |> Maybe.withDefault []
            , Select.init "climbingRouteFormSectorId"
            )
        }


climbingRouteFormSectorSelectConfig : ClimbingRouteFormSettings msg -> Session.Model -> Select.Config msg Sector
climbingRouteFormSectorSelectConfig settings session =
    let
        r : Select.RequiredConfig msg Sector
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = \sector -> sector.name ++ " [" ++ DA.getAreaNameSafe session.data sector.areaId ++ "]"
            , onSelect = settings.onSelect
            , toMsg = settings.selectToMsg
            }
    in
    Select.newConfig r
        |> Select.withPrompt "Sector"


viewClimbingRouteForm : ClimbingRouteFormSettings msg -> Model model -> H.Html msg
viewClimbingRouteForm settings model =
    let
        form =
            Tuple.first model.climbingRouteForm
    in
    H.form [ A.css [ TW.space_y_1 ] ]
        [ Criterium.formTextCriterium "Name" .name Forms.updateName settings.onUpdate form
        , Criterium.formTextCriterium "Grade" .grade Forms.updateGrade settings.onUpdate form
        , Criterium.formSelectionWithSearchCriterium "Sector"
            (climbingRouteFormSectorSelectConfig settings model.session)
            .sectorId
            (Dict.values model.session.data.sectors)
            form
        , Criterium.formTextAreaCriterium "Comment" .comment Forms.updateComment settings.onUpdate form
        , Criterium.formTextAreaCriterium "Beta" .beta Forms.updateBeta settings.onUpdate form
        , Criterium.formSelectionCriterium "Kind"
            (\_ -> List.map Data.climbingRouteKindToString Data.climbingRouteKindEnum)
            Forms.updateKind
            settings.onUpdate
            .kind
            form
        , H.button [ A.type_ "button", E.onClick settings.onSave ] [ H.text "Save" ]
        , Forms.viewErrors form
        ]


validateClimbingRouteForm : Model model -> ( ClimbingRouteForm, Maybe ClimbingRoute )
validateClimbingRouteForm model =
    let
        form =
            Tuple.first model.climbingRouteForm
    in
    Form.succeed ValidatedClimbingRouteFormValues form
        |> Form.append
            (\_ -> Ok <| Forms.idForForm model.session.data.climbingRoutes (Tuple.second model.climbingRouteForm))
        |> Form.append
            (Forms.validateNonEmpty .name "Route can't have an empty name")
        |> Form.append
            (Forms.validateNonEmpty .grade "Route can't have no grade")
        |> Form.append
            (Forms.validateOptional .comment)
        |> Form.append (Forms.validateOptional .beta)
        |> Form.append
            (.kind >> Data.climbingRouteKindFromString >> Result.fromMaybe "A valid routeKind must be selected")
        |> Form.append
            (.sectorId >> Tuple.first >> List.head >> Maybe.map .id >> Result.fromMaybe "A valid sector must be selected")
        |> climbingRouteFromForm


climbingRouteFromForm : ClimbingRouteForm -> ( ClimbingRouteForm, Maybe ClimbingRoute )
climbingRouteFromForm form =
    case form of
        Valid result values ->
            ( Idle values
            , Just <|
                ClimbingRoute result.id result.sectorId result.name result.grade result.comment result.beta result.kind []
            )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        Idle _ ->
            ( form, Nothing )
