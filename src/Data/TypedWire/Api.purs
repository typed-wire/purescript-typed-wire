module Data.TypedWire.Api where

import Prelude
import Data.Maybe
import Data.Argonaut

type ApiHeader =
    { key :: String
    , value :: String
    }

type ApiRequest req =
    { headers :: Array ApiHeader
    , method :: String
    , body :: Maybe Json
    , url :: String
    }

type ApiResponse resp =
    { body :: Maybe Json
    , statusCode :: Int
    }

type ApiCall m req resp = (ApiRequest req -> m (ApiResponse resp)) -> m resp