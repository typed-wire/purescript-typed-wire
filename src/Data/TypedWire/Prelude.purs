module Data.TypedWire.Prelude
    ( module Data.Argonaut
    , module Data.Argonaut.Encode
    , module Data.Argonaut.Decode
    , module Data.Maybe
    , module Data.Array
    , (.??), eatBool
    , DateTime(..), Day(..), TimeOfDay(..)
    , AsBase64(..)
    )
where

import Data.Argonaut
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Array
import Data.Maybe
import Data.Date hiding (fromString)
import qualified Data.Date as D
import Data.Date.UTC
import Data.Time
import qualified Data.String.Regex as R
import Data.Int as I
import Data.Either
import Data.StrMap as M
import Prelude

foreign import dateToISO :: JSDate -> String

newtype AsBase64 = AsBase64 String

unAsBase64 :: AsBase64 -> String
unAsBase64 (AsBase64 d) = d

instance decodeAsBase64 :: DecodeJson AsBase64 where
    decodeJson json = do
        AsBase64 <$> decodeJson json

instance encodeAsBase64 :: EncodeJson AsBase64 where
    encodeJson = encodeJson <<< unAsBase64

newtype DateTime = DateTime Date

unDateTime :: DateTime -> Date
unDateTime (DateTime d) = d

instance decodeDateTime :: DecodeJson DateTime where
    decodeJson json = do
        str <- decodeJson json
        case D.fromString str of
            Nothing -> fail "Inavlid datetime"
            Just d -> pure (DateTime d)

instance encodeDateTime :: EncodeJson DateTime where
    encodeJson = encodeJson <<< dateToISO <<< toJSDate <<< unDateTime

newtype Day = Day Date

dayRegex :: R.Regex
dayRegex =
    R.regex "([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})" R.noFlags

dayFromString :: String -> Maybe Day
dayFromString str = do
    matches <- R.match dayRegex str
    case matches of
        [Just ys, Just ms, Just ds] -> do
            y <- I.fromString ys
            m <- I.fromString ms >>= monthFromInt
            d <- I.fromString ds
            Day <$> date (Year y) m (DayOfMonth d)
        _ -> Nothing

monthFromInt :: Int -> Maybe Month
monthFromInt i =
    case i of
        1 -> Just January
        2 -> Just February
        3 -> Just March
        4 -> Just April
        5 -> Just May
        6 -> Just June
        7 -> Just July
        8 -> Just August
        9 -> Just September
        10 -> Just October
        11 -> Just November
        12 -> Just December
        _ -> Nothing

monthToInt :: Month -> Int
monthToInt m =
    case m of
        January -> 1
        February -> 2
        March -> 3
        April -> 4
        May -> 5
        June -> 6
        July -> 7
        August -> 8
        September -> 9
        October -> 10
        November -> 11
        December -> 12

dayToString :: Day -> String
dayToString (Day dt) =
    show y ++ "-" ++ show m ++ show d
    where
        y = unYear $ year dt
        m = monthToInt $ month dt
        d = unDay $ dayOfMonth dt
        unYear (Year i) = i
        unDay (DayOfMonth i) = i

instance decodeDay :: DecodeJson Day where
    decodeJson json = do
        str <- decodeJson json
        case dayFromString str of
            Nothing -> fail "Inavlid day"
            Just d -> pure d

instance encodeDay :: EncodeJson Day where
    encodeJson = encodeJson <<< dayToString

newtype TimeOfDay =
    TimeOfDay
    { hour :: HourOfDay
    , minute :: MinuteOfHour
    , second :: SecondOfMinute
    }

timeOfDayRegex :: R.Regex
timeOfDayRegex =
    R.regex "([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})" R.noFlags

timeOfDayFromString :: String -> Maybe TimeOfDay
timeOfDayFromString str = do
    matches <- R.match timeOfDayRegex str
    case matches of
        [Just hs, Just ms, Just ss] -> do
            h <- I.fromString hs
            m <- I.fromString ms
            s <- I.fromString ss
            pure $ TimeOfDay { hour: HourOfDay h, minute: MinuteOfHour m, second: SecondOfMinute s }
        _ -> Nothing

timeOfDayToString :: TimeOfDay -> String
timeOfDayToString (TimeOfDay { hour: (HourOfDay h), minute: (MinuteOfHour m), second: (SecondOfMinute s) }) =
    show h ++ ":" ++ show m ++ show s

instance decodeTimeOfDay :: DecodeJson TimeOfDay where
    decodeJson json = do
        str <- decodeJson json
        case timeOfDayFromString str of
            Nothing -> fail "Inavlid time of day"
            Just tod -> pure tod

instance encodeTimeOfDay :: EncodeJson TimeOfDay where
    encodeJson = encodeJson <<< timeOfDayToString

(.??) :: forall a. (DecodeJson a) => JObject -> String -> Either String (Maybe a)
(.??) o s =
    case M.lookup s o of
        Nothing -> pure Nothing
        Just val -> Just <$> decodeJson val

eatBool :: Boolean -> Either String Unit
eatBool _ = pure unit