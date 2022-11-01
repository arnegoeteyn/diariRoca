module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Command
import Data exposing (Data)
import DataParser exposing (encodedJsonFile, jsonFileDecoder)
import Date exposing (Date)
import Dict
import Html.Styled as H
import Json.Decode exposing (decodeString)
import Navbar
import Page.AscentsPage as Ascents
import Page.ClimbingRoute as ClimbingRoute
import Page.ClimbingRoutes as ClimbingRoutes
import Page.Sectors as Sectors
import Page.Stats as Stats
import Session exposing (Route(..))
import Skeleton
import Time
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, oneOf, s, top)



-- Main


main : Program { storageCache : String, posixTime : Int, version : String } Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : ( Session.Route, Page )
    , appState : AppState
    , startUpDate : Date
    , version : String

    -- , modal : ModalContent
    , settingsOpen : Bool
    , googleDriveAuthorized : Bool
    }


type Page
    = NotFoundPage Session.Model
    | ClimbingRoutePage ClimbingRoute.Model
    | ClimbingRoutesPage ClimbingRoutes.Model
    | AscentsPage Ascents.Model
    | SectorsPage Sectors.Model
    | StatsPage Session.Model


type AppState
    = Ready



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Command.loadCache JsonLoaded, Command.googleDriveSubscriptionPort GoogleDriveResponse ]


view : Model -> Browser.Document Msg
view model =
    let
        ( sessionRoute, page ) =
            model.page
    in
    case page of
        NotFoundPage _ ->
            Skeleton.view never
                NavbarMsg
                NotFoundRoute
                { title = "Not Found"
                , warning = Skeleton.NoProblems
                , kids = [ H.text "not found" ]
                , session = exit model
                }

        ClimbingRoutePage climbingRoute ->
            Skeleton.view ClimbingRouteMsg NavbarMsg sessionRoute (ClimbingRoute.view climbingRoute)

        ClimbingRoutesPage climbingRoutes ->
            Skeleton.view ClimbingRoutesMsg NavbarMsg sessionRoute (ClimbingRoutes.view climbingRoutes)

        AscentsPage ascentsModel ->
            Skeleton.view AscentsMsg NavbarMsg sessionRoute (Ascents.view ascentsModel)

        SectorsPage sectorsModel ->
            Skeleton.view SectorsMsg NavbarMsg sessionRoute (Sectors.view sectorsModel)

        StatsPage session ->
            Skeleton.view SectorsMsg NavbarMsg sessionRoute (Stats.view session)



-- INIT


init : { storageCache : String, posixTime : Int, version : String } -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ({ storageCache, posixTime, version } as flags) url key =
    let
        date =
            Date.fromPosix Time.utc (Time.millisToPosix posixTime)

        decodedStorage =
            decodeString jsonFileDecoder storageCache

        jsonFile =
            Result.withDefault { climbingRoutes = Dict.empty, ascents = Dict.empty, sectors = Dict.empty, areas = Dict.empty, trips = Dict.empty } <| decodedStorage
    in
    stepUrl url
        { key = key
        , url = url
        , page = ( NotFoundRoute, NotFoundPage <| Session.init flags )
        , appState = Ready
        , startUpDate = date
        , version = version
        , settingsOpen = False
        , googleDriveAuthorized = False
        }



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | JsonLoaded String
    | GoogleDriveResponse { type_ : String, argument : Maybe String }
    | ClimbingRouteMsg ClimbingRoute.Msg
    | ClimbingRoutesMsg ClimbingRoutes.Msg
    | AscentsMsg Ascents.Msg
    | SectorsMsg Sectors.Msg
    | NavbarMsg Navbar.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        JsonLoaded content ->
            loadContent model content

        GoogleDriveResponse response ->
            case String.toLower response.type_ of
                "authorized" ->
                    ( { model | googleDriveAuthorized = True }, Cmd.none )

                "filechosen" ->
                    Maybe.map (loadContent model) response.argument |> Maybe.withDefault ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClimbingRouteMsg msg ->
            case model.page of
                ( _, ClimbingRoutePage climbingRoute ) ->
                    stepClimbingRoute model (ClimbingRoute.update msg climbingRoute)

                _ ->
                    ( model, Cmd.none )

        ClimbingRoutesMsg msg ->
            case model.page of
                ( _, ClimbingRoutesPage climbingRoutes ) ->
                    stepClimbingRoutes model (ClimbingRoutes.update msg climbingRoutes)

                _ ->
                    ( model, Cmd.none )

        AscentsMsg msg ->
            case model.page of
                ( _, AscentsPage ascentsModel ) ->
                    stepAscents model (Ascents.update msg ascentsModel)

                _ ->
                    ( model, Cmd.none )

        SectorsMsg msg ->
            case model.page of
                ( _, SectorsPage sectorsModel ) ->
                    stepSectors model (Sectors.update msg sectorsModel)

                _ ->
                    ( model, Cmd.none )

        NavbarMsg msg ->
            ( model, Cmd.none )


loadContent : Model -> String -> ( Model, Cmd msg )
loadContent model content =
    let
        result =
            decodeString jsonFileDecoder content
    in
    case result of
        Ok file ->
            let
                data =
                    { climbingRoutes = file.climbingRoutes
                    , ascents = file.ascents
                    , sectors = file.sectors
                    , areas = file.areas
                    , trips = file.trips
                    }
            in
            ( { model
                | appState = Ready

                -- , data = data
              }
            , Cmd.none
            )

        Err _ ->
            ( { model | appState = Ready }, Cmd.none )


stepClimbingRoute : Model -> ( ClimbingRoute.Model, Cmd ClimbingRoute.Msg ) -> ( Model, Cmd Msg )
stepClimbingRoute model ( climbingRoute, cmds ) =
    ( { model | page = ( ClimbingRouteRoute, ClimbingRoutePage climbingRoute ) }
    , Cmd.map ClimbingRouteMsg cmds
    )


stepClimbingRoutes : Model -> ( ClimbingRoutes.Model, Cmd ClimbingRoutes.Msg ) -> ( Model, Cmd Msg )
stepClimbingRoutes model ( climbingRoutes, cmds ) =
    ( { model | page = ( ClimbingRoutesRoute, ClimbingRoutesPage climbingRoutes ) }
    , Cmd.map ClimbingRoutesMsg cmds
    )


stepAscents : Model -> ( Ascents.Model, Cmd Ascents.Msg ) -> ( Model, Cmd Msg )
stepAscents model ( ascentsModel, cmds ) =
    ( { model | page = ( AscentsRoute, AscentsPage ascentsModel ) }
    , Cmd.map AscentsMsg cmds
    )


stepSectors : Model -> ( Sectors.Model, Cmd Sectors.Msg ) -> ( Model, Cmd Msg )
stepSectors model ( sectorsModel, cmds ) =
    ( { model | page = ( SectorsRoute, SectorsPage sectorsModel ) }
    , Cmd.map SectorsMsg cmds
    )



-- ROUTER


exit : Model -> Session.Model
exit model =
    case model.page of
        ( _, NotFoundPage session ) ->
            session

        ( _, ClimbingRoutePage m ) ->
            m.session

        ( _, ClimbingRoutesPage m ) ->
            m.session

        ( _, AscentsPage m ) ->
            m.session

        ( _, SectorsPage m ) ->
            m.session

        ( _, StatsPage session ) ->
            session


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        parser =
            oneOf
                [ route top
                    (stepClimbingRoutes model
                        (ClimbingRoutes.init session)
                    )
                , route (s "routes" </> route_)
                    (\climbingRouteId ->
                        stepClimbingRoute model
                            (ClimbingRoute.init session climbingRouteId)
                    )
                , route (s "ascents")
                    (stepAscents model (Ascents.init session))
                , route (s "sectors")
                    (stepSectors model (Sectors.init session))
                , route (s "stats")
                    ( { model | page = ( StatsRoute, StatsPage session ) }, Cmd.none )
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = ( NotFoundRoute, NotFoundPage session ) }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


route_ : Parser (Int -> a) a
route_ =
    custom "route" String.toInt
