module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Command
import Date exposing (Date)
import Html.Styled as H
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
    , route : Page
    , navbarModel : Navbar.Model
    , appState : AppState
    , startUpDate : Date
    , version : String
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
    Sub.batch
        [ Command.googleDriveSubscriptionPort GoogleDriveResponse
        ]


view : Model -> Browser.Document Msg
view model =
    let
        session =
            exit model

        navbar =
            H.map NavbarMsg <| Navbar.view <| (\x -> { x | session = session }) model.navbarModel
    in
    case model.route of
        NotFoundPage _ ->
            Skeleton.view never
                navbar
                { title = "Not Found"
                , warning = Skeleton.NoProblems
                , kids = [ H.text "not found" ]
                , session = exit model
                }

        ClimbingRoutePage climbingRoute ->
            Skeleton.view ClimbingRouteMsg navbar (ClimbingRoute.view climbingRoute)

        ClimbingRoutesPage climbingRoutes ->
            Skeleton.view ClimbingRoutesMsg navbar (ClimbingRoutes.view climbingRoutes)

        AscentsPage ascentsModel ->
            Skeleton.view AscentsMsg navbar (Ascents.view ascentsModel)

        SectorsPage sectorsModel ->
            Skeleton.view SectorsMsg navbar (Sectors.view sectorsModel)

        StatsPage statsSesssion ->
            Skeleton.view SectorsMsg navbar (Stats.view statsSesssion)



-- INIT


init : { storageCache : String, posixTime : Int, version : String } -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ({ storageCache, posixTime, version } as flags) url key =
    let
        session =
            Session.init flags Session.NotFoundRoute

        date =
            Date.fromPosix Time.utc (Time.millisToPosix posixTime)
    in
    stepUrl url
        { key = key
        , url = url
        , route = NotFoundPage session
        , navbarModel = Navbar.init session
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
      -- Commands
    | GoogleDriveResponse { type_ : String, argument : Maybe String }
      -- Pages
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

        GoogleDriveResponse response ->
            case String.toLower response.type_ of
                "authorized" ->
                    let
                        newSession oldSession =
                            { oldSession | googleDriveAuthorized = True }
                    in
                    ( swapSession model (newSession (exit model)), Cmd.none )

                "filechosen" ->
                    let
                        newSession =
                            response.argument
                                |> Maybe.map (\s -> Session.loadJson s (exit model))
                                |> Maybe.map (Tuple.mapFirst (swapSession model))
                    in
                    Maybe.withDefault ( model, Cmd.none ) newSession

                _ ->
                    ( model, Cmd.none )

        ClimbingRouteMsg msg ->
            case model.route of
                ClimbingRoutePage climbingRoute ->
                    stepClimbingRoute model (ClimbingRoute.update msg climbingRoute)

                _ ->
                    ( model, Cmd.none )

        ClimbingRoutesMsg msg ->
            case model.route of
                ClimbingRoutesPage climbingRoutes ->
                    stepClimbingRoutes model (ClimbingRoutes.update msg climbingRoutes)

                _ ->
                    ( model, Cmd.none )

        AscentsMsg msg ->
            case model.route of
                AscentsPage ascentsModel ->
                    stepAscents model (Ascents.update msg ascentsModel)

                _ ->
                    ( model, Cmd.none )

        SectorsMsg msg ->
            case model.route of
                SectorsPage sectorsModel ->
                    stepSectors model (Sectors.update msg sectorsModel)

                _ ->
                    ( model, Cmd.none )

        NavbarMsg msg ->
            let
                ( navbarModel, cmd ) =
                    Navbar.update msg model.navbarModel
            in
            ( swapSession { model | navbarModel = navbarModel } navbarModel.session, Cmd.map NavbarMsg cmd )


stepClimbingRoute : Model -> ( ClimbingRoute.Model, Cmd ClimbingRoute.Msg ) -> ( Model, Cmd Msg )
stepClimbingRoute model ( climbingRoute, cmds ) =
    ( { model | route = ClimbingRoutePage climbingRoute }
    , Cmd.map ClimbingRouteMsg cmds
    )


stepClimbingRoutes : Model -> ( ClimbingRoutes.Model, Cmd ClimbingRoutes.Msg ) -> ( Model, Cmd Msg )
stepClimbingRoutes model ( climbingRoutes, cmds ) =
    ( { model | route = ClimbingRoutesPage climbingRoutes }
    , Cmd.map ClimbingRoutesMsg cmds
    )


stepAscents : Model -> ( Ascents.Model, Cmd Ascents.Msg ) -> ( Model, Cmd Msg )
stepAscents model ( ascentsModel, cmds ) =
    ( { model | route = AscentsPage ascentsModel }
    , Cmd.map AscentsMsg cmds
    )


stepSectors : Model -> ( Sectors.Model, Cmd Sectors.Msg ) -> ( Model, Cmd Msg )
stepSectors model ( sectorsModel, cmds ) =
    ( { model | route = SectorsPage sectorsModel }
    , Cmd.map SectorsMsg cmds
    )



-- ROUTER


exit : Model -> Session.Model
exit model =
    case model.route of
        NotFoundPage session ->
            session

        ClimbingRoutePage m ->
            m.session

        ClimbingRoutesPage m ->
            m.session

        AscentsPage m ->
            m.session

        SectorsPage m ->
            m.session

        StatsPage session ->
            session


swapSession : Model -> Session.Model -> Model
swapSession model newSession =
    { model
        | route =
            case model.route of
                NotFoundPage _ ->
                    NotFoundPage newSession

                ClimbingRoutePage climbingRoutePage ->
                    ClimbingRoutePage { climbingRoutePage | session = newSession }

                ClimbingRoutesPage climbingRoutesPage ->
                    ClimbingRoutesPage { climbingRoutesPage | session = newSession }

                AscentsPage ascentsPage ->
                    AscentsPage { ascentsPage | session = newSession }

                SectorsPage sectorsPage ->
                    SectorsPage { sectorsPage | session = newSession }

                StatsPage _ ->
                    StatsPage newSession
    }


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        parser =
            oneOf
                [ route top
                    (stepClimbingRoutes model
                        (ClimbingRoutes.init { session | route = Session.ClimbingRoutesRoute })
                    )
                , route (s "routes" </> route_)
                    (\climbingRouteId ->
                        stepClimbingRoute model
                            (ClimbingRoute.init { session | route = Session.ClimbingRouteRoute } climbingRouteId)
                    )
                , route (s "ascents")
                    (stepAscents model (Ascents.init { session | route = Session.AscentsRoute }))
                , route (s "sectors")
                    (stepSectors model (Sectors.init { session | route = Session.SectorsRoute }))
                , route (s "stats")
                    ( { model | route = StatsPage { session | route = Session.StatsRoute } }, Cmd.none )
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | route = NotFoundPage session }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


route_ : Parser (Int -> a) a
route_ =
    custom "route" String.toInt
