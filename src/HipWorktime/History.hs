{-# LANGUAGE DeriveGeneric #-}

module HipWorktime.History
       (
         User(User),
         Message(Message),
         History,
         fetchHistory,
       ) where

import Data.Time.Calendar (Day)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (
  TimeZone, ZonedTime (ZonedTime), TimeOfDay (TimeOfDay),
  LocalTime (LocalTime), getZonedTime, zonedTimeToUTC
  )
import Network.HTTP.Conduit (simpleHttp)
import System.Locale (defaultTimeLocale)
import Data.Aeson (FromJSON, decode)
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Internal (ByteString)

-- ユーザ (id, name)
data User = User {
  name :: String,
  user_id :: Int
  } deriving (Show, Generic)

-- メッセージ
data Message = Message {
  date :: ZonedTime,
  from :: User,
  message :: String
  } deriving (Show, Generic)

data History = History {
  messages :: [Message]
  } deriving (Show, Generic)

instance FromJSON History
instance FromJSON Message
instance FromJSON User

{-
履歴を取得するための URL 文字列を得る。
-}
historyUrl :: String -> String -> Day -> TimeZone -> String
historyUrl token room day zone =
  let
    dayStr = formatTime defaultTimeLocale "%Y-%m-%d" day
    zoneStr = formatTime defaultTimeLocale "%Z" zone
  in
   "https://api.hipchat.com/v1/rooms/history?room_id=" ++ room ++
   "&date=" ++ dayStr ++ "&timezone=" ++ zoneStr ++
   "&auth_token=" ++ token

{-
現在時刻 now において day がすでに過ぎた日であるか？
-}
isOverDay :: ZonedTime -> Day -> TimeZone -> Bool
isOverDay now day zone =
  let
    endOfDay = ZonedTime (LocalTime day (TimeOfDay 23 59 60)) zone
  in
   zonedTimeToUTC now > zonedTimeToUTC endOfDay

{-
特定日の履歴を得る。
-}
fetchHistory :: String -> String -> Day -> TimeZone -> IO [Message]
fetchHistory token room day zone =
  let
    urlStr = historyUrl token room day zone
  in
   do
     now <- getZonedTime
     let
       -- 現在時刻が指定日を超えていたら、キャッシュを作成する
       makeCache = isOverDay now day zone
     resp <- cachedSimpleHttp urlStr makeCache
     case (decode resp :: Maybe History) of
       Just hist -> return $ messages hist
       Nothing -> return []

{-
url にアクセスして結果を返す。
ただしキャッシュがあればそれを優先して返す。
makeCache が True ならば今回の結果をキャッシュする。
-}
cachedSimpleHttp :: String -> Bool -> IO ByteString
cachedSimpleHttp url makeCache =
  -- TODO use cache
  simpleHttp url
