module Data.TypedWire.Api where

import Prelude
import Data.Maybe
import Data.Either
import Data.Argonaut

data ApiMethod
  = DELETE
  | GET
  | HEAD
  | OPTIONS
  | PATCH
  | POST
  | PUT
  | MOVE
  | COPY
  | CustomMethod String

type ApiHeader =
    { key :: String
    , value :: String
    }

type ApiRequest req =
    { headers :: Array ApiHeader
    , method :: ApiMethod
    , body :: Maybe Json
    , url :: String
    }

type ApiResponse resp =
    { body :: Json
    , statusCode :: Int
    }

type ApiCall m req resp = (ApiRequest req -> m (ApiResponse resp)) -> m (Either String resp)