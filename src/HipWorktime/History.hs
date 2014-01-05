{-# LANGUAGE DeriveGeneric #-}

module HipWorktime.History
       (
         User(User),
         Message(Message),
         History,
         fetchHistory,
         getCacheFilePath
       ) where

import Data.Time.Calendar (Day)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (
  TimeZone, ZonedTime (ZonedTime), TimeOfDay (TimeOfDay),
  LocalTime (LocalTime), zonedTimeToUTC
  )
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.HTTP.Conduit (simpleHttp)
import System.Locale (defaultTimeLocale)
import System.Directory (
  getHomeDirectory, createDirectoryIfMissing, doesFileExist
  )
import Data.Aeson (FromJSON, decode)
import GHC.Generics (Generic)
import Data.Digest.Pure.SHA (sha256, showDigest)
import qualified Data.ByteString.Lazy.Char8 as C (
  ByteString, pack, fromStrict, toStrict
  )
import qualified Data.ByteString as BS (readFile, writeFile)
import Control.Monad (when)

-- ユーザ (id, name)
data User = User {
  name :: String,
  user_id :: Int
  } deriving (Show, Generic, Eq)

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

instance Eq Message where
  (==) m1 m2 =
    zonedTimeToUTC (date m1) == zonedTimeToUTC (date m2) &&
    from m1 == from m2 && message m1 == message m2

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
isOverDay :: UTCTime -> Day -> TimeZone -> Bool
isOverDay now day zone =
  let
    endOfDay = ZonedTime (LocalTime day (TimeOfDay 23 59 60)) zone
  in
   now > zonedTimeToUTC endOfDay

{-
特定日の履歴を得る。
-}
fetchHistory :: String -> String -> Day -> TimeZone -> IO [Message]
fetchHistory token room day zone =
  let
    urlStr = historyUrl token room day zone
  in
   do
     now <- getCurrentTime
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
cachedSimpleHttp :: String -> Bool -> IO C.ByteString
cachedSimpleHttp url makeCache = do
  cached <- readCache url
  case cached of
    Just contents -> return contents
    Nothing -> do
      contents <- simpleHttp url
      when makeCache (writeCache url contents)
      return contents
{-
URLに対応するキャッシュを読み込む。
-}
readCache :: String -> IO (Maybe C.ByteString)
readCache url = do
  cacheFilePath <- getCacheFilePath url
  exists <- doesFileExist cacheFilePath
  case exists of
    True -> do
      contents <- BS.readFile cacheFilePath
      return $ Just $ C.fromStrict contents
    False -> return Nothing

{-
URLに対応するキャッシュを書き込む。
-}
writeCache :: String -> C.ByteString -> IO()
writeCache url contents = do
  createCacheDir
  cacheFilePath <- getCacheFilePath url
  BS.writeFile cacheFilePath $ C.toStrict contents

{-|
キャッシュ用ディレクトリを取得する。
-}
getCacheDir :: IO FilePath
getCacheDir = do
  home <- getHomeDirectory
  return $ home ++ "/.hipworktime-hs/cache"

createCacheDir :: IO ()
createCacheDir = do
  cacheDir <- getCacheDir
  createDirectoryIfMissing True cacheDir

getCacheFilePath :: String -> IO FilePath
getCacheFilePath url = do
  cacheDir <- getCacheDir
  return $ cacheDir ++ "/" ++ (showDigest $ sha256 (C.pack url))

