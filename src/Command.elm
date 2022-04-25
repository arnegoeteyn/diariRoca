port module Command exposing (..)

import Json.Encode


port storeCache : Json.Encode.Value -> Cmd msg
