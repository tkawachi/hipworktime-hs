{-# LANGUAGE DeriveGeneric #-}

module HipWorktime.History
       (
         User(User),
         Message(Message),
         History,
         fetchHistory,
       ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
--import Network.HTTP
import Network.HTTP.Conduit
import System.Locale
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics (Generic)

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
特定日の履歴を得る。
-}
fetchHistory :: String -> String -> Day -> TimeZone -> IO [Message]
fetchHistory token room day zone =
  let
    urlStr = historyUrl token room day zone
  in
   do
     resp <- simpleHttp urlStr
     case (decode resp :: Maybe History) of
       Just hist -> return $ messages hist
       Nothing -> return []

{-
キャッシュするファイルのパスを返す。
url を hash して key にする方法が簡単かな。
-}
cachePath :: String -> Day -> TimeZone -> FilePath
cachePath room day zone = "abc"
       
{-
与えられた日付に該当するメッセージ一覧と、与えられた日付のメッセージがすべて
キャッシュ内に存在していたかを返す。
-}
readHistoryCache :: String -> Day -> TimeZone -> IO ([Message], Bool)
readHistoryCache room day zone =
  undefined


-- ファイルにキャッシュされたメッセージを読み込む
readCache :: FilePath -> IO [Message]
readCache path = undefined

-- ファイルにメッセージをキャッシュする
writeCache :: FilePath -> [Message] -> IO ()
writeCache path messages = undefined
