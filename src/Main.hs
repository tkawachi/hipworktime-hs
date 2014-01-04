import Data.Time.Calendar
import Data.Time.LocalTime
--import Network.HTTP
import System.Environment
import System.FilePath
import Text.Read

import HipWorktime.History
import HipWorktime.Config

-- 時刻範囲
data TimeRange = TimeRange LocalTime LocalTime deriving (Show)

{-
日に対応するメッセージを取得する。
システムのタイムゾーンを用いる。
-}
fetchMessages :: Day -> IO [Message]
fetchMessages day = do
  (token, room, uid) <- readConfig
  zone <- getCurrentTimeZone
  messages <- fetchHistory token room day zone
  return (filter isMine messages)
    where
      isMine message = case message of
        Message _ (User uid _) _ -> True
        _ -> False

{-
時刻範囲を取り出す
-}
extractTimeRange :: [Message] -> [TimeRange]
extractTimeRange = undefined

{-
時刻範囲を正規化する
-}
normalize :: [TimeRange] -> [TimeRange]
normalize = undefined

{-
コマンドライン引き数で指定された月の1日を返す
-}
getDayFromArg :: IO (Maybe Day)
getDayFromArg = do
  maybeInts <- fmap (map readMaybe) getArgs :: IO [Maybe Int]
  case maybeInts of
    [Just y, Just m] -> return (fromGregorianValid (toInteger y) m 1)
    _ -> return Nothing

{-
与えられた日付の月の日付リストを返す。
-}
getMonthDays :: Day -> [Day]
getMonthDays day =
  let
    (y, m, d) = toGregorian day
    monthLen = gregorianMonthLength y m
  in
   map (fromGregorian y m) [1..monthLen]

{-
fetch :: IO String
fetch = do
      rsp <- Network.HTTP.simpleHTTP (getRequest "http://www.google.co.jp/")
              -- fetch document and return it (as a 'String'.)
      getResponseBody rsp
      -- fmap (take 100) (getResponseBody rsp)
-}

main :: IO()
main =  do
  day <- getDayFromArg
  case day of
    Just d -> do
       messagess <- mapM fetchMessages $ getMonthDays d
       let
         timeRangess = map extractTimeRange messagess
         normalss = map normalize timeRangess in
         print normalss

    Nothing -> putStrLn "Invalid day. Pass year and month."
