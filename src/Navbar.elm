module Navbar exposing (Model, Msg, init, update, view)

import Command
import Css
import DataParser exposing (encodedJsonFile)
import File
import File.Download
import File.Select
import Html exposing (Html)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Encode exposing (encode)
import Material.Icons.Round as IconsRound
import Material.Icons.Types exposing (Coloring(..))
import Session exposing (ModelEncapsulated, Route(..))
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Task
import Utilities
import View.Button exposing (withMsg)
import View.Icon exposing (iconButton)



-- Model


type alias ModelContent =
    { dropdownOpen : Bool
    }


type alias Model =
    ModelEncapsulated ModelContent



-- Init


init : Session.Model -> Model
init session =
    { dropdownOpen = False
    , session = session
    }



-- Update


type Msg
    = NoOp
    | ToggleDropdown
    | CloseDropdown
    | JsonRequested
    | JsonSelected File.File
    | JsonLoaded String
    | ExportRequested
    | GoogleDriveJsonRequested
    | GoogleDriveExportRequested
    | AuthorizeGoogleDrive


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleDropdown ->
            ( { model | dropdownOpen = not model.dropdownOpen }, Cmd.none )

        CloseDropdown ->
            ( { model | dropdownOpen = True }, Cmd.none )

        JsonRequested ->
            ( model, File.Select.file [ "application/json" ] JsonSelected )

        JsonSelected file ->
            ( model, Task.perform JsonLoaded (File.toString file) )

        JsonLoaded content ->
            Session.loadJson content model.session
                |> Session.assign model

        ExportRequested ->
            let
                result =
                    encode 5 <|
                        encodedJsonFile model.session.data
            in
            ( model, File.Download.string "result.json" "application/json" result )

        AuthorizeGoogleDrive ->
            ( model, Command.googleDriveCommand Command.Authorize )

        GoogleDriveJsonRequested ->
            ( model, Command.googleDriveCommand Command.ShowPicker )

        GoogleDriveExportRequested ->
            let
                result =
                    encode 5 <| encodedJsonFile model.session.data
            in
            ( model, Command.googleDriveCommand (Command.Save result) )



-- View


view : Model -> Html Msg
view model =
    let
        logo =
            H.div [ A.css [ Tw.flex, Tw.items_center, Tw.flex_shrink_0, Tw.text_white, Tw.mr_6 ] ] [ H.text "Diari roca" ]

        links =
            H.div [ A.css [ Tw.flex, Tw.items_center, Tw.w_auto ] ]
                [ navLink ClimbingRoutesRoute { url = "/", caption = "Routes" }
                , navLink AscentsRoute { url = "/ascents", caption = "Ascents" }
                , navLink SectorsRoute { url = "/sectors", caption = "Sectors" }
                , navLink StatsRoute { url = "/stats", caption = "Stats" }
                ]

        navAttributes =
            [ A.css
                [ Tw.flex
                , Tw.flex_col
                , Tw.bg_purple_400
                , Tw.p_6
                , B.lg [ Tw.flex_row ]
                ]
            , A.id "navbar"
            ]

        isActive route =
            case ( route, model.session.route ) of
                ( ClimbingRoutesRoute, ClimbingRouteRoute ) ->
                    True

                _ ->
                    model.session.route == route

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            H.a
                [ A.href url
                , A.css <|
                    [ Tw.block, Tw.mt_4, Tw.mr_4, Tw.text_purple_200, B.lg [ Tw.inline_block, Tw.mt_0 ] ]
                        ++ Utilities.filterAndReplaceList [ ( Tw.underline, isActive route, Just Tw.no_underline ) ]
                ]
                [ H.text caption ]
    in
    H.nav navAttributes
        [ logo
        , links
        , dropDown model
        ]


dropDown : Model -> Html Msg
dropDown model =
    let
        versionLink =
            H.a
                [ A.css
                    [ Tw.block
                    , Tw.py_2
                    , Tw.px_4
                    , Tw.bg_gray_100
                    , Tw.text_gray_700
                    , Tw.text_xs
                    , Css.hover
                        [ Tw.bg_gray_600, Tw.text_white ]
                    ]
                , A.href ("https://github.com/arnegoeteyn/diariRoca/releases/tag/v" ++ model.session.version)
                , A.target "_blank"
                ]
                [ H.text ("v" ++ model.session.version) ]
    in
    H.li [ A.css [ Tw.block ] ]
        [ iconButton IconsRound.settings ToggleDropdown
        , H.div [ A.css [ Tw.bg_white, Tw.absolute ], A.css <| Utilities.filterList [ ( Tw.hidden, not model.dropdownOpen ) ] ]
            (List.map
                (\( action, text ) ->
                    H.div
                        [ E.onClick action
                        , A.css
                            [ Tw.block
                            , Tw.py_2
                            , Tw.px_4
                            , Tw.bg_gray_100
                            , Tw.text_gray_700
                            , Css.hover
                                [ Tw.bg_gray_600, Tw.text_white ]
                            ]
                        ]
                        [ H.text text ]
                )
                ([ ( JsonRequested, "Load JSON" )
                 , ( ExportRequested, "Save JSON" )
                 ]
                    ++ (if model.session.googleDriveAuthorized then
                            [ ( GoogleDriveJsonRequested, "Load JSON from Google Drive" )
                            , ( GoogleDriveExportRequested, "Save JSON to Google Drive" )
                            ]

                        else
                            [ ( AuthorizeGoogleDrive, "Authorize Google Drive" ) ]
                       )
                )
                ++ [ versionLink ]
            )
        ]
