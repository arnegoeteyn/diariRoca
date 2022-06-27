port module Command exposing (GoogleDriveCommandType(..), googleDriveCommand, googleDriveCommandPort, googleDriveSubscriptionPort, loadCache, storeCache)

import Json.Encode
import Message exposing (Msg)


port storeCache : Json.Encode.Value -> Cmd msg


port loadCache : (String -> msg) -> Sub msg


type GoogleDriveCommandType
    = Authorize
    | ShowPicker
    | Save String


googleDriveCommandTypeToCommand : GoogleDriveCommandType -> { type_ : String, argument : Maybe String }
googleDriveCommandTypeToCommand request =
    case request of
        Authorize ->
            { type_ = "Authorize", argument = Nothing }

        ShowPicker ->
            { type_ = "ShowPicker", argument = Nothing }

        Save content ->
            { type_ = "Save", argument = Just content }


googleDriveCommand : GoogleDriveCommandType -> Cmd msg
googleDriveCommand =
    googleDriveCommandPort << googleDriveCommandTypeToCommand


port googleDriveCommandPort : { type_ : String, argument : Maybe String } -> Cmd msg


port googleDriveSubscriptionPort : ({ type_ : String, argument : Maybe String } -> msg) -> Sub msg
