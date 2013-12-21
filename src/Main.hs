import Data.Time.Calendar
import Data.Time.LocalTime
import Network.HTTP
import System.Environment
import System.FilePath
import Text.Read

import HipWorktime.History
import HipWorktime.Config

-- 時刻範囲
data TimeRange = TimeRange LocalTime LocalTime

{-
日に対応するメッセージを取得する。
システムのタイムゾーンを用いる。
-}
fetchMessages :: Day -> IO [Message]
fetchMessages day = do
  (token, room) <- readConfig
  return [Message (LocalTime day (TimeOfDay 0 0 0)) (User "a" "hoge") "aaa"]

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
getDay :: IO (Maybe Day)
getDay = do
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

fetch :: IO String
fetch = do
      rsp <- Network.HTTP.simpleHTTP (getRequest "http://www.google.co.jp/")
              -- fetch document and return it (as a 'String'.)
      getResponseBody rsp
      -- fmap (take 100) (getResponseBody rsp)

main :: IO()
main =  do
  day <- getDay
  case day of
    Just d -> do
       messages <- mapM fetchMessages $ getMonthDays d
       print messages
    Nothing -> putStrLn "Invalid day. Input year and month."
