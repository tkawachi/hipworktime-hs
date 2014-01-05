import Data.Time.Calendar (
  Day, fromGregorianValid, toGregorian, gregorianMonthLength, fromGregorian
  )
import Data.Time.LocalTime (LocalTime, getCurrentTimeZone)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import HipWorktime.History (Message (Message), fetchHistory, User (User))
import HipWorktime.Config (readConfig)

-- 時刻範囲
data TimeRange = TimeRange LocalTime LocalTime deriving (Show)

{-
メッセージが自分のものか？
-}
isMyMessage :: String -> Message -> Bool
isMyMessage name message = case message of
  Message _ (User name' _) _ | name' == name -> True
  _ -> False

{-
開始、終了を表すメッセージか？
-}
isInOutMessage :: Message -> Bool
isInOutMessage message = case message of
  Message _ _ "in" -> True
  Message _ _ "out" -> True
  _ -> False

{-
日に対応する自分のメッセージを取得する。
システムのタイムゾーンを用いる。
-}
fetchMessages :: Day -> IO [Message]
fetchMessages day = do
  (token, room, uid) <- readConfig
  zone <- getCurrentTimeZone
  messages <- fetchHistory token room day zone
  return $ filter (isMyMessage uid) messages

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
    (y, m, _) = toGregorian day
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
       print messagess
       let
         timeRangess = map extractTimeRange messagess
         normalss = map normalize timeRangess
       print normalss

    Nothing -> putStrLn "Invalid day. Pass year and month."
