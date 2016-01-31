module Data.TypedWire.Api where

import Data.TypedWire.Prelude

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

class PathPiece s where
    toPathPiece :: s -> String

instance stringPathPiece :: PathPiece String where
    toPathPiece = id

instance intPathPiece :: PathPiece Int where
    toPathPiece = show

instance numberPathPiece :: PathPiece Number where
    toPathPiece = show

instance boolPathPiece :: PathPiece Boolean where
    toPathPiece = show