module Page.Sectors exposing (..)

import Data exposing (Area, Data, Sector)
import DataAccessors as DA
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
import Utilities exposing (orElse)
import View.Button as Button



-- Model


type alias ModelContent =
    { selectedArea : Maybe Area
    , modal : ModalContent
    , areaForm : ( AreaForm, Maybe Area )
    , sectorForm : ( SectorForm, Maybe Sector )
    }


type alias Model =
    Session.ModelEncapsulated ModelContent


type ModalContent
    = Empty
    | DeleteAreaRequestModal Area
    | DeleteSectorRequestModal Sector
    | AreaFormModal
    | SectorFormModal


type alias AreaFormValues =
    { name : String
    , country : String
    }


type alias ValidatedAreaFormValues =
    { name : String
    , country : String
    , id : Int
    }


type alias AreaForm =
    Form.Form AreaFormValues ValidatedAreaFormValues


type alias SectorFormValues =
    { name : String
    , areaId : SelectionCriterium Area
    }


type alias ValidatedSectorFormValues =
    { name : String
    , areaId : Int
    , id : Int
    }


type alias SectorForm =
    Form.Form SectorFormValues ValidatedSectorFormValues



-- Init


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , modal = Empty
      , selectedArea = Nothing
      , areaForm = ( initAreaForm Nothing, Nothing )
      , sectorForm = ( initSectorForm session.data Nothing Nothing, Nothing )
      }
    , Cmd.none
    )


initAreaForm : Maybe Area -> AreaForm
initAreaForm maybeArea =
    Form.Idle
        { name = Maybe.map .name maybeArea |> Maybe.withDefault ""
        , country = Maybe.map .country maybeArea |> Maybe.withDefault ""
        }


initSectorForm : Data -> Maybe Area -> Maybe Sector -> SectorForm
initSectorForm data maybeArea maybeSector =
    let
        areaId =
            Maybe.map .areaId maybeSector
                |> orElse (Maybe.map .id maybeArea)
                |> Maybe.andThen (DA.getArea data)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    Form.Idle
        { name = Maybe.map .name maybeSector |> Maybe.withDefault ""
        , areaId =
            ( areaId
            , Select.init "sectorFormAreaId"
            )
        }


sectorFormAreaSelectConfig : Select.Config Msg Area
sectorFormAreaSelectConfig =
    let
        r : Select.RequiredConfig Msg Area
        r =
            { filter = \x y -> DU.filterAreasByName x y |> Utilities.listToMaybe
            , toLabel = .name
            , onSelect = SectorFormSelectArea
            , toMsg = SectorFormSelectAreaMsg
            }
    in
    Select.newConfig r



-- Update


type Msg
    = NoOp
    | CloseModal
    | AreaSelected (Maybe Area)
      -- AreaForm
    | OpenAreaForm (Maybe Area)
    | DeleteAreaRequested Area
    | DeleteAreaConfirmation Area
    | UpdateAreaForm AreaForm
    | SaveAreaForm
      -- SectorForm
    | OpenSectorForm (Maybe Sector)
    | DeleteSectorRequested Sector
    | DeleteSectorConfirmation Sector
    | UpdateSectorForm SectorForm
    | SectorFormSelectArea (Maybe Area)
    | SectorFormSelectAreaMsg (Select.Msg Area)
    | SaveSectorForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Empty }, Cmd.none )

        -- Data - Area
        OpenAreaForm maybeArea ->
            ( { model
                | modal = AreaFormModal
                , areaForm = ( initAreaForm maybeArea, maybeArea )
              }
            , Cmd.none
            )

        DeleteAreaRequested area ->
            ( { model | modal = DeleteAreaRequestModal area }, Cmd.none )

        DeleteAreaConfirmation area ->
            Session.assign model <| Session.deleteArea area model.session

        UpdateAreaForm values ->
            ( { model | areaForm = Utilities.replaceFirst values model.areaForm }, Cmd.none )

        SaveAreaForm ->
            let
                ( newForm, maybeArea ) =
                    validateAreaForm model

                areaForm_ =
                    Utilities.replaceFirst newForm model.areaForm
            in
            case maybeArea of
                Just area ->
                    Session.addArea area model.session
                        |> Session.assign { model | areaForm = areaForm_, modal = Empty }

                _ ->
                    ( { model | areaForm = areaForm_ }, Cmd.none )

        -- Sectors
        OpenSectorForm maybeSector ->
            ( { model | modal = SectorFormModal, sectorForm = ( initSectorForm model.session.data model.selectedArea maybeSector, maybeSector ) }, Cmd.none )

        DeleteSectorRequested sector ->
            ( { model | modal = DeleteSectorRequestModal sector }, Cmd.none )

        DeleteSectorConfirmation sector ->
            Session.assign model <| Session.deleteSector sector model.session

        UpdateSectorForm values ->
            ( { model | sectorForm = Utilities.replaceFirst values model.sectorForm }, Cmd.none )

        SectorFormSelectArea maybeArea ->
            let
                newForm =
                    \f ->
                        { f
                            | areaId =
                                Tuple.mapFirst
                                    (\_ -> Maybe.withDefault [] <| Maybe.map List.singleton maybeArea)
                                    f.areaId
                        }
            in
            ( { model | sectorForm = Tuple.mapFirst (Form.mapValues newForm) model.sectorForm }, Cmd.none )

        SectorFormSelectAreaMsg subMsg ->
            let
                ( updatedForm, cmd ) =
                    updateSelectCriteriumMsg .areaId (\x v -> { v | areaId = x }) sectorFormAreaSelectConfig subMsg (Tuple.first model.sectorForm)
            in
            ( { model
                | sectorForm =
                    Utilities.replaceFirst updatedForm model.sectorForm
              }
            , cmd
            )

        SaveSectorForm ->
            let
                ( newForm, maybeSector ) =
                    validateSectorForm model

                sectorForm_ =
                    Utilities.replaceFirst newForm model.sectorForm
            in
            case maybeSector of
                Just sector ->
                    Session.addSector sector model.session
                        |> Session.assign { model | sectorForm = sectorForm_, modal = Empty }

                _ ->
                    ( { model | sectorForm = sectorForm_ }, Cmd.none )

        AreaSelected area ->
            ( { model | selectedArea = area }, Cmd.none )



--View


view : Model -> Skeleton.Details Msg
view model =
    { title = "Sectors"
    , warning = Skeleton.NoProblems
    , session = model.session
    , kids =
        let
            modal =
                Modal.view CloseModal NoOp
        in
        [ H.div [ A.css [ TW.grid, TW.grid_cols_2 ] ]
            [ viewAreas { selectedArea = Nothing } (Utilities.dictToList model.session.data.areas |> DU.sortAreas)
            , viewSectors { selectedSector = Nothing } (filterSectors model)
            ]
        , case model.modal of
            Empty ->
                H.text ""

            DeleteSectorRequestModal sector ->
                modal (deleteSectorConfirmation model sector)

            DeleteAreaRequestModal area ->
                modal (deleteAreaConfirmation model area)

            AreaFormModal ->
                modal (areaFormModal model)

            SectorFormModal ->
                modal (sectorFormModal model)
        ]
    }


viewAreas : { selectedArea : Maybe Area } -> List Area -> H.Html Msg
viewAreas options areas =
    H.div
        []
        [ H.text "Areas"
        , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenAreaForm Nothing))
        , H.div []
            (List.map
                (\area ->
                    H.div
                        [ A.id <| "area-" ++ String.fromInt area.id
                        , A.css [ TW.border, TW.border_solid, TW.py_4 ]
                        , E.onClick (AreaSelected (Just area))
                        ]
                        [ viewArea { area = area, selected = False } ]
                )
                areas
            )
        ]


viewArea : { area : Area, selected : Bool } -> Html Msg
viewArea { area, selected } =
    H.div
        [ A.css [ TW.flex, TW.place_content_between, TW.mx_8 ]
        ]
        [ H.text <| area.name
        , H.div []
            [ Button.editButton (Button.defaultOptions |> Button.withMsg (OpenAreaForm (Just area)) |> Button.withKind Button.Icon)
            , Button.deleteButton (Button.defaultOptions |> Button.withMsg (DeleteAreaRequested area) |> Button.withKind Button.Icon)
            ]
        ]


viewSectors : { selectedSector : Maybe Sector } -> List Sector -> H.Html Msg
viewSectors options sectors =
    H.div []
        [ H.text "Sectors"
        , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenSectorForm Nothing))
        , H.div []
            (List.map
                (\sector ->
                    H.div
                        [ A.id <| "sector-" ++ String.fromInt sector.id
                        , A.css [ TW.border, TW.border_solid, TW.py_4 ]
                        , E.onClick NoOp
                        ]
                        [ viewSectorRow { sector = sector, selected = False } ]
                )
                sectors
            )
        ]


viewSectorRow : { sector : Sector, selected : Bool } -> Html Msg
viewSectorRow { sector, selected } =
    H.div
        [ A.css [ TW.flex, TW.place_content_between, TW.mx_8 ]
        ]
        [ H.div [ A.css [] ] [ H.text <| sector.name ]
        , H.div []
            [ Button.editButton (Button.defaultOptions |> Button.withMsg (OpenSectorForm (Just sector)) |> Button.withKind Button.Icon)
            , Button.deleteButton (Button.defaultOptions |> Button.withMsg (DeleteSectorRequested sector) |> Button.withKind Button.Icon)
            , Button.gotoButton
                (Button.defaultOptions
                    |> Button.withHref ("sectors/" ++ String.fromInt sector.id)
                    |> Button.withKind Button.Icon
                )
            ]
        ]



-- Utilities


filterSectors : Model -> List Sector
filterSectors model =
    case model.selectedArea of
        Nothing ->
            Utilities.dictToList model.session.data.sectors

        Just area ->
            DU.filterSectorsByAreaId area.id (Utilities.dictToList model.session.data.sectors)


areaFormModal : Model -> Html Msg
areaFormModal model =
    areaForm model


deleteAreaConfirmation : Model -> Area -> Html Msg
deleteAreaConfirmation model area =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete \"", area.name, "\" " ] ]
        , H.button [ E.onClick <| DeleteAreaConfirmation area ] [ H.text "confirm" ]
        ]


sectorFormModal : Model -> Html Msg
sectorFormModal model =
    sectorForm model


deleteSectorConfirmation : Model -> Sector -> Html Msg
deleteSectorConfirmation model sector =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete \"", sector.name, "\" " ] ]
        , H.button [ E.onClick <| DeleteSectorConfirmation sector ] [ H.text "confirm" ]
        ]


areaForm : Model -> H.Html Msg
areaForm model =
    let
        form =
            Tuple.first model.areaForm
    in
    H.form []
        [ formTextCriterium "Name" .name updateName UpdateAreaForm form
        , formTextCriterium "Country" .country updateCountry UpdateAreaForm form
        , H.button [ A.type_ "button", E.onClick SaveAreaForm ] [ H.text "Save" ]
        , viewErrors form
        ]


validateAreaForm : Model -> ( AreaForm, Maybe Area )
validateAreaForm model =
    let
        form =
            Tuple.first model.areaForm
    in
    Form.succeed ValidatedAreaFormValues form
        |> Form.append
            (validateNonEmpty .name "Area can't have an empty name")
        |> Form.append
            (validateNonEmpty .country "Area must belong to a country")
        |> Form.append
            (\_ -> Ok <| idForForm model.session.data.areas (Tuple.second model.areaForm))
        |> areaFromForm model


areaFromForm : Model -> AreaForm -> ( AreaForm, Maybe Area )
areaFromForm _ form =
    case form of
        Form.Valid areaValues values ->
            ( Form.Idle values, Just <| Area areaValues.id areaValues.name areaValues.country )

        Form.Invalid errors values ->
            ( Form.Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )



--| Sector


sectorForm : Model -> H.Html Msg
sectorForm model =
    let
        form =
            Tuple.first model.sectorForm
    in
    H.form []
        [ formTextCriterium "Name" .name updateName UpdateSectorForm form
        , formSelectionWithSearchCriterium "Area" sectorFormAreaSelectConfig .areaId (Dict.values model.session.data.areas) form
        , H.button [ A.type_ "button", E.onClick SaveSectorForm ] [ H.text "Save" ]
        , viewErrors form
        ]


validateSectorForm : Model -> ( SectorForm, Maybe Sector )
validateSectorForm model =
    let
        form =
            Tuple.first model.sectorForm
    in
    Form.succeed ValidatedSectorFormValues form
        |> Form.append
            (validateNonEmpty .name "Sector can't have an empty name")
        |> Form.append
            (.areaId >> Tuple.first >> List.head >> Maybe.map .id >> Result.fromMaybe "A valid area must be selected")
        |> Form.append
            (\_ -> Ok <| idForForm model.session.data.sectors (Tuple.second model.sectorForm))
        |> sectorFromForm model


sectorFromForm : Model -> SectorForm -> ( SectorForm, Maybe Sector )
sectorFromForm _ form =
    case form of
        Form.Valid sectorValues values ->
            ( Form.Idle values, Just <| Sector sectorValues.id sectorValues.areaId sectorValues.name )

        Form.Invalid errors values ->
            ( Form.Invalid errors values, Nothing )

        _ ->
            ( form, Nothing )


updateCountry : a -> { b | country : a } -> { b | country : a }
updateCountry value form =
    { form | country = value }
