module Page.ClimbingRoute exposing (Model, Msg(..), init, update, view)

import Data exposing (ClimbingRoute, ClimbingRouteKind, Sector)
import DataAccessors as DA
import DataUtilities
import Date
import Dict
import Forms.Criterium exposing (formSelectionCriterium, formSelectionWithSearchCriterium, formTextAreaCriterium, formTextCriterium, textCriterium)
import Forms.Form as Form exposing (Form(..))
import Forms.Forms exposing (SelectionCriterium, idForForm, validateNonEmpty, validateOptional)
import General
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Modal
import Select
import Skeleton
import Tailwind.Utilities as Tw
import Utilities
import View.Button as Button



-- Model


type alias Model =
    { mediaLink : String
    , mediaLabel : String
    , routeId : Int
    , climbingRouteForm : ( ClimbingRouteForm, Maybe ClimbingRoute )
    , modal : ModalContent
    }


type ModalContent
    = Empty
    | ClimbingRouteFormModal
    | DeleteClimbingRouteRequestModal ClimbingRoute



-- Init


init : Int -> ( Model, Cmd Msg )
init id =
    ( { mediaLink = ""
      , mediaLabel = ""
      , routeId = id
      , climbingRouteForm = ( initClimbingRouteForm Nothing Nothing, Nothing )
      , modal = Empty
      }
    , Cmd.none
    )


initClimbingRouteForm : Maybe General.Model -> Maybe ClimbingRoute -> ClimbingRouteForm
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



-- Update


type Msg
    = NoOp
    | SetMediaLink String
    | SetMediaLabel String
    | UpdateClimbingRouteForm ClimbingRouteForm
    | OpenClimbingRouteForm (Maybe ClimbingRoute)
    | ClimbingRouteFormSelectSector (Maybe Sector)
    | ClimbingRouteFormSelectSectorMsg (Select.Msg Sector)
    | SaveClimbingRouteForm
    | DeleteClimbingRouteRequested ClimbingRoute
    | CloseModal
      -- General
    | DeleteClimbingRouteConfirmation ClimbingRoute


update : Msg -> Model -> General.Model -> ( Model, Cmd Msg, General.Msg )
update msg model general =
    let
        removeFromSelected item =
            List.filter (\c -> c /= item)
    in
    case msg of
        SetMediaLink link ->
            ( { model | mediaLink = link }, Cmd.none, General.None )

        SetMediaLabel label ->
            ( { model | mediaLabel = label }, Cmd.none, General.None )

        OpenClimbingRouteForm maybeClimbingRoute ->
            ( { model
                | modal = ClimbingRouteFormModal
                , climbingRouteForm = ( initClimbingRouteForm (Just general) maybeClimbingRoute, maybeClimbingRoute )
              }
            , Cmd.none
            , General.None
            )

        UpdateClimbingRouteForm values ->
            ( { model | climbingRouteForm = Utilities.replaceFirst values model.climbingRouteForm }, Cmd.none, General.None )

        ClimbingRouteFormSelectSector maybeSector ->
            let
                newForm =
                    \f ->
                        { f
                            | sectorId =
                                Tuple.mapFirst
                                    (\_ -> Maybe.withDefault [] <| Maybe.map List.singleton maybeSector)
                                    f.sectorId
                        }
            in
            ( { model | climbingRouteForm = Tuple.mapFirst (Form.mapValues newForm) model.climbingRouteForm }, Cmd.none, General.None )

        ClimbingRouteFormSelectSectorMsg subMsg ->
            let
                ( updatedForm, cmd ) =
                    updateSelectCriteriumMsg .sectorId
                        (\x v -> { v | sectorId = x })
                        (climbingRouteFormSectorSelectConfig general)
                        subMsg
                        (Tuple.first model.climbingRouteForm)
            in
            ( { model
                | climbingRouteForm =
                    Utilities.replaceFirst updatedForm model.climbingRouteForm
              }
            , cmd
            , General.None
            )

        SaveClimbingRouteForm ->
            -- Todo
            ( model, Cmd.none, General.None )

        DeleteClimbingRouteRequested climbingRoute ->
            ( { model | modal = DeleteClimbingRouteRequestModal climbingRoute }
            , Cmd.none
            , General.None
            )

        DeleteClimbingRouteConfirmation climbingRoute ->
            ( model, Cmd.none, General.DeleteClimbingRouteConfirmation climbingRoute )

        NoOp ->
            General.withNothing ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Empty }, Cmd.none, General.None )



-- View


view : Model -> General.Model -> Skeleton.Details Msg
view model general =
    let
        maybeRoute =
            DA.getClimbingRoute general.data model.routeId
    in
    { title = "Climbing Route"
    , header = []
    , warning = Skeleton.NoProblems
    , kids =
        let
            modal =
                Modal.view CloseModal NoOp
        in
        [ case maybeRoute of
            Just route ->
                H.div []
                    [ H.h1 [] [ H.text route.name ]
                    , viewRouteDetail model general route
                    , case model.modal of
                        Empty ->
                            H.text ""

                        ClimbingRouteFormModal ->
                            modal [ viewClimbingRouteFormModal model general ]

                        DeleteClimbingRouteRequestModal climbingRoute ->
                            modal [ viewDeleteClimbingRouteConfirmation climbingRoute ]
                    ]

            Nothing ->
                H.text "route not found"
        ]
    }


viewRouteDetail : Model -> General.Model -> ClimbingRoute -> Html Msg
viewRouteDetail model general route =
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
            , viewAscentsList model general route
            ]
        , H.div [ A.css [] ]
            [ viewRouteImage route
            , viewRouteMedia model route
            ]
        , H.div []
            [ Button.deleteButton (Button.defaultOptions |> Button.withMsg (DeleteClimbingRouteRequested route) |> Button.withKind Button.TextAndIcon)
            , Button.editButton (Button.defaultOptions |> Button.withMsg (OpenClimbingRouteForm (Just route)) |> Button.withKind Button.TextAndIcon)
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


viewDeleteClimbingRouteConfirmation : ClimbingRoute -> Html Msg
viewDeleteClimbingRouteConfirmation route =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete \"", route.name, "\" " ] ]
        , H.button [ E.onClick <| DeleteClimbingRouteConfirmation route ] [ H.text "confirm" ]
        ]


viewClimbingRouteFormModal : Model -> General.Model -> Html Msg
viewClimbingRouteFormModal model general =
    H.div []
        [ H.h2 [] [ H.text "New climbingroute" ], climbingRouteForm model general ]


viewAscentsList : Model -> General.Model -> ClimbingRoute -> Html Msg
viewAscentsList model general route =
    let
        ascents =
            DA.getAscents general.data route
    in
    H.div [ A.css [] ]
        [ H.h3 [ A.css [] ]
            [ H.text (Utilities.stringFromList [ String.fromInt <| List.length ascents, " ascents:" ])

            -- , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenAscentForm Nothing route))
            ]
        , H.div [ A.css [ Tw.divide_solid, Tw.divide_y_2, Tw.divide_x_0 ] ] <|
            List.map
                (\ascent ->
                    H.div [ A.css [ Tw.p_1_dot_5 ] ]
                        [ H.div [ A.css [ Tw.grid, Tw.grid_cols_3 ] ]
                            [ H.div [ A.css [] ] [ H.text <| Date.toIsoString ascent.date ]
                            , H.div [ A.css [] ] [ H.text (Data.ascentKindToString ascent.kind) ]
                            , H.div
                                []
                                [--  Button.deleteButton (Button.defaultOptions |> Button.withMsg (Message.DeleteAscentRequested ascent))
                                 -- , Button.editButton (Button.defaultOptions |> Button.withMsg (Message.OpenAscentForm (Just ascent) route))
                                ]
                            ]
                        , H.div [ A.css [] ] [ H.text <| Maybe.withDefault "" ascent.comment ]
                        ]
                )
                ascents
        ]



-- | ClimbingRoute


type alias ClimbingRouteForm =
    Form ClimbingRouteFormValues ValidatedClimbingRouteFormValues


type alias ClimbingRouteFormValues =
    { name : String
    , grade : String
    , comment : String
    , beta : String
    , sectorId : SelectionCriterium Sector
    , kind : String
    }


type alias ValidatedClimbingRouteFormValues =
    { id : Int
    , name : String
    , grade : String
    , comment : Maybe String
    , beta : Maybe String
    , kind : ClimbingRouteKind
    , sectorId : Int
    }


climbingRouteForm : Model -> General.Model -> H.Html Msg
climbingRouteForm model general =
    let
        form =
            Tuple.first model.climbingRouteForm
    in
    H.form [ A.css [ Tw.space_y_1 ] ]
        [ formTextCriterium "Name" .name updateName UpdateClimbingRouteForm form
        , formTextCriterium "Grade" .grade updateGrade UpdateClimbingRouteForm form
        , formSelectionWithSearchCriterium "Sector" (climbingRouteFormSectorSelectConfig general) .sectorId (Dict.values general.data.sectors) form
        , formTextAreaCriterium "Comment" .comment updateComment UpdateClimbingRouteForm form
        , formTextAreaCriterium "Beta" .beta updateBeta UpdateClimbingRouteForm form
        , formSelectionCriterium "Kind"
            (\_ -> List.map Data.climbingRouteKindToString Data.climbingRouteKindEnum)
            updateKind
            UpdateClimbingRouteForm
            .kind
            form
        , H.button [ A.type_ "button", E.onClick SaveClimbingRouteForm ] [ H.text "Save" ]
        , Forms.Forms.viewErrors form
        ]


climbingRouteFormSectorSelectConfig : General.Model -> Select.Config Msg Sector
climbingRouteFormSectorSelectConfig general =
    let
        r : Select.RequiredConfig Msg Sector
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = \sector -> sector.name ++ " [" ++ DA.getAreaNameSafe general.data sector.areaId ++ "]"
            , onSelect = ClimbingRouteFormSelectSector
            , toMsg = ClimbingRouteFormSelectSectorMsg
            }
    in
    Select.newConfig r
        -- |> Select.withOnRemoveItem OnRemoveSectorSelection
        |> Select.withPrompt "Sector"


validateClimbingRouteForm : Model -> General.Model -> ( ClimbingRouteForm, Maybe ClimbingRoute )
validateClimbingRouteForm model general =
    let
        form =
            Tuple.first model.climbingRouteForm
    in
    Form.succeed ValidatedClimbingRouteFormValues form
        |> Form.append
            (\_ -> Ok <| idForForm general.data.climbingRoutes (Tuple.second model.climbingRouteForm))
        |> Form.append
            (validateNonEmpty .name "Route can't have an empty name")
        |> Form.append
            (validateNonEmpty .grade "Route can't have no grade")
        |> Form.append
            (validateOptional .comment)
        |> Form.append (validateOptional .beta)
        |> Form.append
            (.kind >> Data.climbingRouteKindFromString >> Result.fromMaybe "A valid routeKind must be selected")
        |> Form.append
            (.sectorId >> Tuple.first >> List.head >> Maybe.map .id >> Result.fromMaybe "A valid sector must be selected")
        |> climbingRouteFromForm


climbingRouteFromForm : ClimbingRouteForm -> ( ClimbingRouteForm, Maybe ClimbingRoute )
climbingRouteFromForm form =
    case form of
        Valid climbingRouteValues values ->
            ( Idle values
            , Just <|
                ClimbingRoute
                    climbingRouteValues.id
                    climbingRouteValues.sectorId
                    climbingRouteValues.name
                    climbingRouteValues.grade
                    climbingRouteValues.comment
                    climbingRouteValues.beta
                    climbingRouteValues.kind
                    []
            )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| UpdateUtilities


updateAreaId : a -> { b | areaId : a } -> { b | areaId : a }
updateAreaId value form =
    { form | areaId = value }


updateBeta : a -> { b | beta : a } -> { b | beta : a }
updateBeta value form =
    { form | beta = value }


updateComment : a -> { b | comment : a } -> { b | comment : a }
updateComment value form =
    { form | comment = value }


updateCountry : a -> { b | country : a } -> { b | country : a }
updateCountry value form =
    { form | country = value }


updateGrade : a -> { b | grade : a } -> { b | grade : a }
updateGrade value form =
    { form | grade = value }


updateKind : a -> { b | kind : a } -> { b | kind : a }
updateKind value form =
    { form | kind = value }


updateName : a -> { b | name : a } -> { b | name : a }
updateName value form =
    { form | name = value }


updateSelectCriteriumMsg : (a -> SelectionCriterium item) -> (SelectionCriterium item -> a -> a) -> Select.Config msg item -> Select.Msg item -> Form a r -> ( Form a r, Cmd msg )
updateSelectCriteriumMsg extractor wrapper config msg =
    Form.mapAndReturn
        (\values ->
            let
                ( updated, selectCmd ) =
                    Select.update config msg (Tuple.second (extractor values))
            in
            ( wrapper (Tuple.mapSecond (\_ -> updated) (extractor values)) values
            , selectCmd
            )
        )
