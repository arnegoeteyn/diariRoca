module Page.ClimbingRoute exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Data exposing (Ascent, AscentKind, ClimbingRoute, ClimbingRouteKind, Media, Sector)
import DataAccessors as DA
import DataUtilities
import Date exposing (Date)
import DatePicker
import Dict
import Forms.Criterium as Criterium exposing (formSelectionCriterium, formSelectionWithSearchCriterium, formTextAreaCriterium, formTextCriterium, textCriterium)
import Forms.Form as Form exposing (Form(..))
import Forms.Forms exposing (DateCriterium, SelectionCriterium, idForForm, validateNonEmpty, validateOptional)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Modal
import Select
import Session
import Skeleton
import Tailwind.Utilities as Tw
import Utilities
import View.Button as Button



-- Model


type alias Model =
    { session : Session.Model
    , mediaLink : String
    , mediaLabel : String
    , routeId : Int
    , climbingRouteForm : ( ClimbingRouteForm, Maybe ClimbingRoute )
    , ascentForm : ( AscentForm, Maybe AscentFormMeta )
    , modal : ModalContent
    }


type ModalContent
    = Empty
    | ClimbingRouteFormModal
    | DeleteClimbingRouteRequestModal ClimbingRoute
    | AscentFormModal
    | DeleteAscentRequestModal Ascent



-- Init


init : Session.Model -> Int -> ( Model, Cmd Msg )
init session id =
    let
        ( ascentForm, ascentFormCmd ) =
            initAscentForm session.startUpDate Nothing
    in
    ( { session = session
      , mediaLink = ""
      , mediaLabel = ""
      , routeId = id
      , climbingRouteForm = ( initClimbingRouteForm Nothing Nothing, Nothing )
      , modal = Empty
      , ascentForm = ( ascentForm, Nothing )
      }
    , ascentFormCmd
    )


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


initAscentForm : Date -> Maybe Ascent -> ( AscentForm, Cmd Msg )
initAscentForm date maybeAscent =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( Idle
        { comment = Maybe.andThen .comment maybeAscent |> Maybe.withDefault ""
        , kind = Data.ascentKindToString <| Maybe.withDefault Data.Onsight <| Maybe.map .kind maybeAscent
        , date = ( Date.toRataDie (Maybe.map .date maybeAscent |> Maybe.withDefault date), datePicker )
        }
    , Cmd.map AscentFormToDatePicker datePickerFx
    )



-- Update


type Msg
    = NoOp
    | CloseModal
      -- Media
    | SetMediaLink String
    | SetMediaLabel String
    | AddMediaToRoute ClimbingRoute
    | RemoveMedia ClimbingRoute Media
      -- Climbing Route actions
    | DeleteClimbingRouteRequested ClimbingRoute
    | DeleteClimbingRouteConfirmation ClimbingRoute
    | OpenClimbingRouteForm (Maybe ClimbingRoute)
      -- Ascent actions
    | OpenAscentForm (Maybe Ascent) ClimbingRoute -- todo do we need ClimbingRoute here?
    | DeleteAscentConfirmation Ascent
    | DeleteAscentRequested Ascent
      -- ClimbingRouteForm
    | UpdateClimbingRouteForm ClimbingRouteForm
    | ClimbingRouteFormSelectSector (Maybe Sector)
    | ClimbingRouteFormSelectSectorMsg (Select.Msg Sector)
    | SaveClimbingRouteForm
      -- Ascent form
    | UpdateAscentForm AscentForm
    | AscentFormToDatePicker DatePicker.Msg
    | SaveAscentForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Empty }, Cmd.none )

        -- Media
        SetMediaLink link ->
            ( { model | mediaLink = link }, Cmd.none )

        SetMediaLabel label ->
            ( { model | mediaLabel = label }, Cmd.none )

        AddMediaToRoute climbingRoute ->
            let
                media =
                    Media model.mediaLink model.mediaLabel
            in
            Session.assign model <| Session.addMediaToRoute climbingRoute media model.session

        RemoveMedia climbingRoute media ->
            Session.assign model <| Session.removeMediaFromRoute climbingRoute media model.session

        -- Climbing route actions
        DeleteClimbingRouteRequested climbingRoute ->
            ( { model | modal = DeleteClimbingRouteRequestModal climbingRoute }
            , Cmd.none
            )

        DeleteClimbingRouteConfirmation climbingRoute ->
            let
                task =
                    Nav.load "/"
            in
            Session.deleteClimbingRoute climbingRoute model
                |> Session.assignCommand task

        OpenClimbingRouteForm maybeClimbingRoute ->
            ( { model
                | modal = ClimbingRouteFormModal
                , climbingRouteForm = ( initClimbingRouteForm (Just model.session) maybeClimbingRoute, maybeClimbingRoute )
              }
            , Cmd.none
            )

        -- Ascent actions
        OpenAscentForm maybeAscent climbingRoute ->
            let
                ( ascentForm, ascentCmd ) =
                    initAscentForm model.session.startUpDate maybeAscent
            in
            ( { model
                | modal = AscentFormModal
                , ascentForm = ( ascentForm, Just ( maybeAscent, climbingRoute ) )
              }
            , ascentCmd
            )

        DeleteAscentRequested ascent ->
            ( { model | modal = DeleteAscentRequestModal ascent }, Cmd.none )

        DeleteAscentConfirmation ascent ->
            Session.assign model <| Session.deleteAscent ascent model.session

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
                        (climbingRouteFormSectorSelectConfig model)
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
                    validateClimbingRouteForm model

                updatedModel =
                    { model
                        | climbingRouteForm = Utilities.replaceFirst newForm model.climbingRouteForm
                    }
            in
            case maybeClimbingRoute of
                Just climbingRoute ->
                    { updatedModel | modal = Empty }
                        |> Session.addClimbingRoute climbingRoute

                Nothing ->
                    ( updatedModel, Cmd.none )

        -- AscentForm
        UpdateAscentForm values ->
            ( { model | ascentForm = Utilities.replaceFirst values model.ascentForm }, Cmd.none )

        SaveAscentForm ->
            let
                ( newForm, maybeAscent ) =
                    validateAscentForm model

                updatedModel =
                    { model
                        | modal = Empty
                        , ascentForm = Utilities.replaceFirst newForm model.ascentForm
                    }
            in
            case maybeAscent of
                Just ascent ->
                    Session.assign updatedModel <| Session.addAscent ascent model.session

                Nothing ->
                    ( updatedModel, Cmd.none )

        AscentFormToDatePicker subMsg ->
            ( { model
                | ascentForm =
                    Tuple.mapFirst
                        (Criterium.updateDateCriterium .date
                            (\x v -> { v | date = x })
                            DatePicker.defaultSettings
                            subMsg
                        )
                        model.ascentForm
              }
            , Cmd.none
            )



-- View


view : Model -> Skeleton.Details Msg
view model =
    let
        maybeRoute =
            DA.getClimbingRoute model.session.data model.routeId
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
                    [ H.h1 [] [ H.text <| route.name ++ " ~ " ++ route.grade ]
                    , viewRouteDetail model route
                    , case model.modal of
                        Empty ->
                            H.text ""

                        ClimbingRouteFormModal ->
                            modal [ viewClimbingRouteFormModal model ]

                        DeleteClimbingRouteRequestModal climbingRoute ->
                            modal [ viewDeleteClimbingRouteConfirmation climbingRoute ]

                        AscentFormModal ->
                            modal [ viewAscentFormModal model ]

                        DeleteAscentRequestModal ascent ->
                            modal [ viewDeleteAscentConfirmation ascent ]
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
            , viewAscentsList model route
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
                , Button.addButton (Button.defaultOptions |> Button.withMsg (AddMediaToRoute route))
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
                            , H.button [ E.onClick <| RemoveMedia route media ] [ H.text "x" ]
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


viewClimbingRouteFormModal : Model -> Html Msg
viewClimbingRouteFormModal model =
    H.div []
        [ H.h2 [] [ H.text "New climbingroute" ], climbingRouteForm model ]


viewAscentFormModal : Model -> Html Msg
viewAscentFormModal model =
    H.div []
        [ H.h2 [] [ H.text "New ascent" ], viewAscentForm model ]


viewDeleteAscentConfirmation : Ascent -> Html Msg
viewDeleteAscentConfirmation ascent =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete" ] ]
        , H.button [ E.onClick <| DeleteAscentConfirmation ascent ] [ H.text "confirm" ]
        ]


viewAscentsList : Model -> ClimbingRoute -> Html Msg
viewAscentsList model route =
    let
        ascents =
            DA.getAscents model.session.data route
    in
    H.div [ A.css [] ]
        [ H.h3 [ A.css [] ]
            [ H.text (Utilities.stringFromList [ String.fromInt <| List.length ascents, " ascents:" ])
            , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenAscentForm Nothing route))
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
                                [ Button.deleteButton (Button.defaultOptions |> Button.withMsg (DeleteAscentRequested ascent))
                                , Button.editButton (Button.defaultOptions |> Button.withMsg (OpenAscentForm (Just ascent) route))
                                ]
                            ]
                        , H.div [ A.css [] ] [ H.text <| Maybe.withDefault "" ascent.comment ]
                        ]
                )
                ascents
        ]



-- ascentForm


type alias AscentFormMeta =
    ( Maybe Ascent, ClimbingRoute )


type alias AscentFormValues =
    { date : DateCriterium
    , kind : String
    , comment : String
    }


type alias ValidatedAscentFormValues =
    { id : Int
    , climbingRouteId : Int
    , date : Date
    , kind : AscentKind
    , comment : Maybe String
    }


type alias AscentForm =
    Form AscentFormValues ValidatedAscentFormValues


viewAscentForm : Model -> H.Html Msg
viewAscentForm model =
    let
        form =
            Tuple.first model.ascentForm
    in
    H.form []
        [ formTextCriterium "Comment" .comment updateComment UpdateAscentForm form
        , formSelectionCriterium "Kind"
            (\_ -> List.map Data.ascentKindToString Data.ascentKindEnum)
            updateKind
            UpdateAscentForm
            .kind
            form
        , Criterium.dateCriterium "Date" DatePicker.defaultSettings .date AscentFormToDatePicker form
        , H.button [ A.type_ "button", E.onClick SaveAscentForm ] [ H.text "Save" ]
        , Forms.Forms.viewErrors form
        ]


validateAscentForm : Model -> ( AscentForm, Maybe Ascent )
validateAscentForm model =
    let
        form =
            Tuple.first model.ascentForm
    in
    case Tuple.second model.ascentForm of
        Nothing ->
            ( form, Nothing )

        Just ( maybeAscent, climbingRoute ) ->
            Form.succeed ValidatedAscentFormValues form
                |> Form.append
                    (\_ -> Ok <| idForForm model.session.data.ascents maybeAscent)
                |> Form.append
                    (\_ -> Ok climbingRoute.id)
                |> Form.append
                    (\values -> Ok <| Date.fromRataDie <| Tuple.first values.date)
                |> Form.append
                    (.kind >> Data.ascentKindFromString >> Result.fromMaybe "A valid ascentKind must be selected")
                |> Form.append (validateOptional .comment)
                |> ascentFromForm


ascentFromForm : AscentForm -> ( AscentForm, Maybe Ascent )
ascentFromForm form =
    case form of
        Valid ascentFormValues values ->
            ( Idle values
            , Just <|
                Ascent ascentFormValues.id
                    ascentFormValues.climbingRouteId
                    ascentFormValues.date
                    ascentFormValues.comment
                    ascentFormValues.kind
            )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        Idle _ ->
            ( form, Nothing )



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


climbingRouteForm : Model -> H.Html Msg
climbingRouteForm model =
    let
        form =
            Tuple.first model.climbingRouteForm
    in
    H.form [ A.css [ Tw.space_y_1 ] ]
        [ formTextCriterium "Name" .name updateName UpdateClimbingRouteForm form
        , formTextCriterium "Grade" .grade updateGrade UpdateClimbingRouteForm form
        , formSelectionWithSearchCriterium "Sector"
            (climbingRouteFormSectorSelectConfig model)
            .sectorId
            (Dict.values model.session.data.sectors)
            form
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


climbingRouteFormSectorSelectConfig : Model -> Select.Config Msg Sector
climbingRouteFormSectorSelectConfig model =
    let
        r : Select.RequiredConfig Msg Sector
        r =
            { filter = \x y -> DataUtilities.filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = \sector -> sector.name ++ " [" ++ DA.getAreaNameSafe model.session.data sector.areaId ++ "]"
            , onSelect = ClimbingRouteFormSelectSector
            , toMsg = ClimbingRouteFormSelectSectorMsg
            }
    in
    Select.newConfig r
        |> Select.withPrompt "Sector"


validateClimbingRouteForm : Model -> ( ClimbingRouteForm, Maybe ClimbingRoute )
validateClimbingRouteForm model =
    let
        form =
            Tuple.first model.climbingRouteForm
    in
    Form.succeed ValidatedClimbingRouteFormValues form
        |> Form.append
            (\_ -> Ok <| idForForm model.session.data.climbingRoutes (Tuple.second model.climbingRouteForm))
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
        Valid result values ->
            ( Idle values
            , Just <|
                ClimbingRoute result.id result.sectorId result.name result.grade result.comment result.beta result.kind []
            )

        Invalid errors values ->
            ( Invalid errors values, Nothing )

        Idle _ ->
            ( form, Nothing )



--| UpdateUtilities


updateBeta : a -> { b | beta : a } -> { b | beta : a }
updateBeta value form =
    { form | beta = value }


updateComment : a -> { b | comment : a } -> { b | comment : a }
updateComment value form =
    { form | comment = value }


updateGrade : a -> { b | grade : a } -> { b | grade : a }
updateGrade value form =
    { form | grade = value }


updateKind : a -> { b | kind : a } -> { b | kind : a }
updateKind value form =
    { form | kind = value }


updateName : a -> { b | name : a } -> { b | name : a }
updateName value form =
    { form | name = value }
