module Page.AscentsPage exposing (Model, Msg(..), init, update, view)

import Css
import Data exposing (Ascent, Trip)
import DataAccessors as MA
import Date exposing (Date)
import DatePicker
import Dict exposing (Dict)
import Form.Criterium exposing (dateCriterium, updateDateCriterium)
import Form.Form as Form exposing (Form)
import Form.Forms exposing (DateCriterium, idForForm, viewErrors)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import List exposing (sortBy)
import Modal
import Session
import Skeleton
import Tailwind.Utilities as Tw
import Utilities
import View.Button as Button



-- Model


type alias ModelContent =
    { modal : ModalContent
    , tripForm : ( TripForm, Maybe Trip )
    }


type alias Model =
    Session.ModelEncapsulated ModelContent


type ModalContent
    = Empty
    | TripOverviewModal Data.Trip
    | TripFormModal


type alias TripFormValues =
    { from : DateCriterium
    , to : DateCriterium
    }


type alias ValidatedTripFormValues =
    { from : Date
    , to : Date
    , id : Int
    }


type alias TripForm =
    Form TripFormValues ValidatedTripFormValues



-- Init


init : Session.Model -> ( Model, Cmd Msg )
init session =
    let
        ( ascentForm, ascentFormCmd ) =
            initTripForm session.startUpDate Nothing
    in
    ( { session = session
      , modal = Empty
      , tripForm = ( ascentForm, Nothing )
      }
    , Cmd.batch [ ascentFormCmd ]
    )


initTripForm : Date -> Maybe Trip -> ( TripForm, Cmd Msg )
initTripForm defaultDate maybeTrip =
    let
        ( fromDatePicker, fromDatePickerFx ) =
            DatePicker.init

        ( toDatePicker, toDatePickerFx ) =
            DatePicker.init
    in
    ( Form.Idle
        { from = ( Date.toRataDie (Maybe.map .from maybeTrip |> Maybe.withDefault defaultDate), fromDatePicker )
        , to = ( Date.toRataDie (Maybe.map .to maybeTrip |> Maybe.withDefault defaultDate), toDatePicker )
        }
    , Cmd.batch
        [ Cmd.map FromTripFormToDatePicker fromDatePickerFx
        , Cmd.map ToTripFormToDatePicker toDatePickerFx
        ]
    )


tripFormDatePickerSettings : DatePicker.Settings
tripFormDatePickerSettings =
    DatePicker.defaultSettings



-- Update
-- View


type Msg
    = NoOp
    | CloseModal
    | OpenTripForm (Maybe Trip)
    | OpenTripOverview Trip
    | UpdateTripForm TripForm
    | FromTripFormToDatePicker DatePicker.Msg
    | ToTripFormToDatePicker DatePicker.Msg
    | SaveTripForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Empty }, Cmd.none )

        OpenTripForm maybeTrip ->
            let
                ( tripForm, tripFormCmd ) =
                    initTripForm model.session.startUpDate maybeTrip
            in
            ( { model | modal = TripFormModal, tripForm = ( tripForm, maybeTrip ) }, tripFormCmd )

        OpenTripOverview trip ->
            ( { model | modal = TripOverviewModal trip }, Cmd.none )

        UpdateTripForm values ->
            ( { model | tripForm = Utilities.replaceFirst values model.tripForm }, Cmd.none )

        FromTripFormToDatePicker subMsg ->
            ( { model
                | tripForm =
                    Tuple.mapFirst
                        (updateDateCriterium .from
                            (\x v -> { v | from = x })
                            tripFormDatePickerSettings
                            subMsg
                        )
                        model.tripForm
              }
            , Cmd.none
            )

        ToTripFormToDatePicker subMsg ->
            ( { model
                | tripForm =
                    Tuple.mapFirst
                        (updateDateCriterium .to
                            (\x v -> { v | to = x })
                            tripFormDatePickerSettings
                            subMsg
                        )
                        model.tripForm
              }
            , Cmd.none
            )

        SaveTripForm ->
            let
                ( newForm, maybeTrip ) =
                    validateTripForm model

                updatedModel =
                    { model
                        | tripForm = Utilities.replaceFirst newForm model.tripForm
                        , modal = Empty
                    }
            in
            case maybeTrip of
                Just trip ->
                    Session.assign updatedModel (Session.addTrip trip model.session)

                _ ->
                    ( updatedModel, Cmd.none )


view : Model -> Skeleton.Details Msg
view model =
    { title = "Ascents"
    , warning = Skeleton.NoProblems
    , session = model.session
    , kids =
        let
            modal =
                Modal.view CloseModal NoOp
        in
        [ H.div []
            [ H.div [ A.css [] ] <|
                List.map
                    (\ascent ->
                        H.div [ A.css [ Tw.flex, Tw.flex_row ], E.onClick NoOp ]
                            [ viewAscentTripIndicator (MA.getTripFromDate model.session.data ascent.date) (tripColorDict model.session.data.trips)
                            , H.div
                                [ A.css [ Tw.flex_grow, Tw.border, Tw.border_solid, Tw.py_4 ]
                                ]
                                [ viewAscentRow model ascent
                                ]
                            ]
                    )
                    (sortedAndFilteredAscents model)
            ]
        , case model.modal of
            Empty ->
                H.text ""

            TripFormModal ->
                modal (viewTripForm model)

            TripOverviewModal trip ->
                modal (tripOverviewModal model trip)
        ]
    }


viewAscentRow : Model -> Ascent -> H.Html Msg
viewAscentRow model ascent =
    let
        maybeRoute =
            MA.getClimbingRoute model.session.data ascent.routeId

        ( routeName, routeGrade ) =
            Maybe.map (\route -> ( route.name, route.grade )) maybeRoute
                |> Maybe.withDefault ( "N/A", "N/A" )
    in
    H.div [ A.css [ Tw.flex ] ]
        [ H.div [ A.css [ Tw.w_2over6 ] ] [ H.text <| Date.toIsoString ascent.date ]
        , H.div [ A.css [ Tw.w_2over6 ] ]
            [ H.text <| Utilities.stringFromList [ routeName, " ", "(", routeGrade, ")" ]
            ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text (Data.ascentKindToString ascent.kind) ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text (Data.ascentKindToString ascent.kind) ]
        , case maybeRoute of
            Just route ->
                Button.gotoButton (Button.defaultOptions |> Button.withHref ("routes/" ++ String.fromInt route.id))

            Nothing ->
                H.text ""
        ]



--| Utilities


sortedAndFilteredAscents : Model -> List Ascent
sortedAndFilteredAscents model =
    Dict.toList model.session.data.ascents
        |> List.map Tuple.second
        |> Utilities.sortByDescending (.date >> Date.toIsoString)


tripColorDict : Dict Int Trip -> Dict Int Css.Style
tripColorDict trips =
    let
        availableColors =
            [ Tw.bg_green_400
            , Tw.bg_red_400
            , Tw.bg_blue_400
            , Tw.bg_pink_400
            , Tw.bg_purple_400
            ]

        difference =
            Dict.size trips // List.length availableColors |> (+) 1

        colors =
            List.repeat difference availableColors |> List.concat
    in
    Utilities.dictToList trips
        |> sortBy (.from >> Date.toRataDie)
        |> List.map .id
        |> (\sorted -> List.map2 Tuple.pair sorted colors)
        |> Dict.fromList


viewAscentTripIndicator : Maybe Trip -> Dict Int Css.Style -> H.Html Msg
viewAscentTripIndicator maybeTrip tripDict =
    let
        color =
            Maybe.andThen (\trip -> Dict.get trip.id tripDict) maybeTrip |> Maybe.withDefault Tw.bg_white

        message =
            case maybeTrip of
                Just trip ->
                    E.onClick (OpenTripOverview trip)

                Nothing ->
                    E.onClick (OpenTripForm Nothing)
    in
    H.div [ A.css [ color, Tw.pl_10 ], message ] []


validateTripForm : Model -> ( TripForm, Maybe Trip )
validateTripForm model =
    let
        form =
            Tuple.first model.tripForm

        verifyFromIsBeforeTo formValues =
            if Tuple.first formValues.from > Tuple.first formValues.to then
                Err "start date can't be after end date"

            else
                Ok never
    in
    Form.succeed ValidatedTripFormValues form
        |> Form.append
            (\values -> Ok <| Date.fromRataDie <| Tuple.first values.from)
        |> Form.append
            (\values -> Ok <| Date.fromRataDie <| Tuple.first values.to)
        |> Form.check verifyFromIsBeforeTo
        |> Form.append
            (\_ -> Ok <| idForForm model.session.data.trips (Tuple.second model.tripForm))
        |> tripFromForm


tripFromForm : TripForm -> ( TripForm, Maybe Trip )
tripFromForm form =
    case form of
        Form.Valid tripFormValues values ->
            ( Form.Idle values
            , Just <|
                Trip tripFormValues.id
                    tripFormValues.from
                    tripFormValues.to
            )

        Form.Invalid errors values ->
            ( Form.Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )


tripOverviewModal : Model -> Trip -> H.Html Msg
tripOverviewModal model trip =
    let
        grades =
            MA.getRoutesFromTrip model.session.data trip
                |> Dict.toList
                |> Utilities.sortByDescending Tuple.first
    in
    H.div
        []
        [ H.text <| Utilities.stringFromListWith " " [ Date.toIsoString trip.from, "-", Date.toIsoString trip.to ]
        , H.div []
            (List.map
                (\( grade, count ) ->
                    H.div []
                        [ H.text <|
                            Utilities.stringFromListWith
                                " "
                                [ grade, "-", String.fromInt count ]
                        ]
                )
                grades
            )
        , H.button [ E.onClick <| OpenTripForm (Just trip) ] [ H.text "edit" ]
        ]


viewTripForm : Model -> H.Html Msg
viewTripForm model =
    let
        form =
            Tuple.first model.tripForm
    in
    H.form []
        [ dateCriterium "Date" tripFormDatePickerSettings .from FromTripFormToDatePicker form
        , dateCriterium "Date" tripFormDatePickerSettings .to ToTripFormToDatePicker form
        , H.button [ A.type_ "button", E.onClick SaveTripForm ] [ H.text "Save" ]
        , viewErrors form
        ]
