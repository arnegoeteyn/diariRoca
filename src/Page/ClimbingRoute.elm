module Page.ClimbingRoute exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Data exposing (Ascent, AscentKind, ClimbingRoute, ClimbingRouteKind, Media, Sector)
import DataAccessors as DA
import Date exposing (Date)
import DatePicker
import Form.Criterium as Criterium exposing (formSelectionCriterium, formSelectionWithSearchCriterium, formTextAreaCriterium, formTextCriterium, textCriterium)
import Form.Form as Form exposing (Form(..))
import Form.Forms as Forms
import Form.Forms.ClimbingRouteForm as ClimbingRouteForm
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


type alias ModelContent =
    { mediaLink : String
    , mediaLabel : String
    , routeId : Int
    , climbingRouteForm : ( ClimbingRouteForm.ClimbingRouteForm, Maybe ClimbingRoute )
    , ascentForm : ( AscentForm, Maybe AscentFormMeta )
    , modal : ModalContent
    }


type alias Model =
    Session.ModelEncapsulated ModelContent


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
      , climbingRouteForm = ( ClimbingRouteForm.initClimbingRouteForm ClimbingRouteForm.emptyValues, Nothing )
      , modal = Empty
      , ascentForm = ( ascentForm, Nothing )
      }
    , ascentFormCmd
    )


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


climbingRouteFormSettings : ClimbingRouteForm.ClimbingRouteFormSettings Msg
climbingRouteFormSettings =
    { onSave = SaveClimbingRouteForm
    , onUpdate = UpdateClimbingRouteForm
    , onSelect = ClimbingRouteFormSelectSector
    , selectToMsg = ClimbingRouteFormSelectSectorMsg
    }



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
    | UpdateClimbingRouteForm ClimbingRouteForm.ClimbingRouteForm
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
            Session.deleteClimbingRoute climbingRoute model.session
                |> Session.assignWithCommand model task

        OpenClimbingRouteForm maybeClimbingRoute ->
            ( { model
                | modal = ClimbingRouteFormModal
                , climbingRouteForm = ( ClimbingRouteForm.initClimbingRouteForm (ClimbingRouteForm.valuesFromMaybeRoute model.session maybeClimbingRoute), maybeClimbingRoute )
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
                    Session.addClimbingRoute climbingRoute model.session
                        |> Session.assign { updatedModel | modal = Empty }

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
    , warning = Skeleton.NoProblems
    , session = model.session
    , kids =
        let
            modal =
                Modal.view CloseModal NoOp
        in
        [ case maybeRoute of
            Just route ->
                H.div []
                    [ viewRouteHeader model route
                    , viewRouteDetail model route
                    , case model.modal of
                        Empty ->
                            H.text ""

                        ClimbingRouteFormModal ->
                            modal (viewClimbingRouteFormModal model)

                        DeleteClimbingRouteRequestModal climbingRoute ->
                            modal (viewDeleteClimbingRouteConfirmation climbingRoute)

                        AscentFormModal ->
                            modal (viewAscentFormModal model)

                        DeleteAscentRequestModal ascent ->
                            modal (viewDeleteAscentConfirmation ascent)
                    ]

            Nothing ->
                H.text "route not found"
        ]
    }


viewRouteHeader : Model -> ClimbingRoute -> Html Msg
viewRouteHeader model route =
    let
        sectorName =
            DA.getSectorNameSafe model.session.data route.sectorId
    in
    H.div []
        [ H.h1 [] [ H.text <| route.name ++ " ~ " ++ route.grade ]
        , H.h2 [] [ H.text sectorName ]
        ]


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
        , H.div [ A.css [ Tw.px_4 ] ]
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
viewRouteImage route =
    let
        hasImageExtension image =
            List.member (String.toLower image) [ "jpg", "png", "jpeg"]

        isImage s =
            String.split "." s |> List.reverse >> List.head |> Maybe.map hasImageExtension |> Maybe.withDefault False

        firstImage =
            Utilities.first (.link >> isImage) route.media
    in
    case firstImage of
        Just image ->
            H.img [ A.src image.link, A.css [ Tw.max_w_full ] ]
                []

        Nothing ->
            H.text "No image found"


viewRouteMedia : Model -> ClimbingRoute -> Html Msg
viewRouteMedia model route =
    let
        hasMedia =
            (not << List.isEmpty) route.media

        addMediaInput =
            H.div []
                [ textCriterium "Link" .mediaLink identity SetMediaLink model
                , textCriterium "Label" .mediaLabel identity SetMediaLabel model
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
        [ H.h2 [] [ H.text "New climbingroute" ], ClimbingRouteForm.viewClimbingRouteForm climbingRouteFormSettings model ]


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
    { date : Forms.DateCriterium
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
        [ formTextCriterium "Comment" .comment Forms.updateComment UpdateAscentForm form
        , formSelectionCriterium "Kind"
            (\_ -> List.map Data.ascentKindToString Data.ascentKindEnum)
            Forms.updateKind
            UpdateAscentForm
            .kind
            form
        , Criterium.dateCriterium "Date" DatePicker.defaultSettings .date AscentFormToDatePicker form
        , H.button [ A.type_ "button", E.onClick SaveAscentForm ] [ H.text "Save" ]
        , Forms.viewErrors form
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
                    (\_ -> Ok <| Forms.idForForm model.session.data.ascents maybeAscent)
                |> Form.append
                    (\_ -> Ok climbingRoute.id)
                |> Form.append
                    (\values -> Ok <| Date.fromRataDie <| Tuple.first values.date)
                |> Form.append
                    (.kind >> Data.ascentKindFromString >> Result.fromMaybe "A valid ascentKind must be selected")
                |> Form.append (Forms.validateOptional .comment)
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
