module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Command
import Data exposing (Data)
import DataParser exposing (encodedJsonFile, jsonFileDecoder)
import Date exposing (Date)
import Dict
import General
import Html.Styled as H
import Json.Decode exposing (decodeString)
import Page.ClimbingRoute as ClimbingRoute
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
        , update = updateWithStorage
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , appState : AppState
    , startUpDate : Date
    , version : String

    -- , modal : ModalContent
    , settingsOpen : Bool
    , googleDriveAuthorized : Bool
    , general : General.Model
    }


type Route
    = NotFoundRoute
    | ClimbingRoute ClimbingRoute.Model


type AppState
    = Ready



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Command.loadCache JsonLoaded, Command.googleDriveSubscriptionPort GoogleDriveResponse ]


view : Model -> Browser.Document Msg
view model =
    case model.route of
        NotFoundRoute ->
            Skeleton.view never
                { title = "Not Found"
                , header = []
                , warning = Skeleton.NoProblems
                , kids = [ H.text "not found" ]
                }

        ClimbingRoute climbingRoute ->
            Skeleton.view ClimbingRouteMsg (ClimbingRoute.view climbingRoute model.general)



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
    General.ignore <|
        stepUrl url
            { key = key
            , url = url
            , route = NotFoundRoute
            , appState = Ready
            , startUpDate = date
            , version = version

            -- , modal = Model.Empty
            , settingsOpen = False
            , googleDriveAuthorized = False
            , general = General.init flags
            }



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | JsonLoaded String
    | GoogleDriveResponse { type_ : String, argument : Maybe String }
    | GeneralMsg General.Msg
    | ClimbingRouteMsg ClimbingRoute.Msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch
        [ Command.storeCache
            (encodedJsonFile newModel.general.data)
        , cmds
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        ( newModel, cmds, generalMsg ) =
            case message of
                NoOp ->
                    ( model, Cmd.none, General.None )

                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model
                            , Nav.pushUrl model.key (Url.toString url)
                            , General.None
                            )

                        Browser.External href ->
                            ( model
                            , Nav.load href
                            , General.None
                            )

                UrlChanged url ->
                    stepUrl url model

                JsonLoaded content ->
                    loadContent model content

                GoogleDriveResponse response ->
                    case String.toLower response.type_ of
                        "authorized" ->
                            ( { model | googleDriveAuthorized = True }, Cmd.none, General.None )

                        "filechosen" ->
                            Maybe.map (loadContent model) response.argument |> Maybe.withDefault ( model, Cmd.none, General.None )

                        _ ->
                            ( model, Cmd.none, General.None )

                GeneralMsg msg ->
                    General.withNothing <| stepGeneral model (General.update msg model.general)

                ClimbingRouteMsg msg ->
                    case model.route of
                        ClimbingRoute climbingRoute ->
                            stepClimbingRoute model (ClimbingRoute.update msg climbingRoute model.general)

                        _ ->
                            ( model, Cmd.none, General.None )

        ( general, generalcmds ) =
            case generalMsg of
                General.None ->
                    ( newModel, Cmd.none )

                _ ->
                    stepGeneral newModel (General.update generalMsg newModel.general)
    in
    ( general, Cmd.batch [ cmds, generalcmds ] )


loadContent : Model -> String -> ( Model, Cmd msg, General.Msg )
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
            , General.None
            )

        Err _ ->
            ( { model | appState = Ready }, Cmd.none, General.None )


stepGeneral : Model -> ( General.Model, Cmd General.Msg ) -> ( Model, Cmd Msg )
stepGeneral model ( general, cmds ) =
    ( { model | general = general }
    , Cmd.map GeneralMsg cmds
    )


stepClimbingRoute : Model -> ( ClimbingRoute.Model, Cmd ClimbingRoute.Msg, General.Msg ) -> ( Model, Cmd Msg, General.Msg )
stepClimbingRoute model ( climbingRoute, cmds, msg ) =
    ( { model | route = ClimbingRoute climbingRoute }
    , Cmd.map ClimbingRouteMsg cmds
    , msg
    )



-- ROUTER


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg, General.Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ -- route top
                  -- (Debug.todo "t")
                  route (s "routes" </> route_)
                    (\climbingRouteId ->
                        let
                            ( m, msg ) =
                                ClimbingRoute.init climbingRouteId
                        in
                        stepClimbingRoute model ( m, msg, General.None )
                    )
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | route = NotFoundRoute }
            , Cmd.none
            , General.None
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


route_ : Parser (Int -> a) a
route_ =
    custom "route" String.toInt
