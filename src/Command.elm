port module Command exposing (storeCache)

import Json.Encode


port storeCache : Json.Encode.Value -> Cmd msg
