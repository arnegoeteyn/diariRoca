port module Command exposing (..)

import Json.Encode
import Message exposing (Msg(..))
import Model exposing (Model)


port storeCache : Json.Encode.Value -> Cmd msg
