import Data.Time.Clock (diffUTCTime, addUTCTime, NominalDiffTime)
import Data.Time.Calendar (
  Day, fromGregorianValid, toGregorian, gregorianMonthLength, fromGregorian
  )
import Data.Time.LocalTime (
  ZonedTime, getCurrentTimeZone, zonedTimeToUTC, utcToZonedTime, zonedTimeZone
  )
import System.Environment (getArgs)
import Text.Read (readMaybe)
import HipWorktime.History (Message (Message), fetchHistory, User (User))
import HipWorktime.Config (readConfig)
import Data.Generics.Aliases (orElse)

{-
時刻範囲。
開始時刻と終了までの秒数で表現する。
-}
data TimeRange = TimeRange ZonedTime NominalDiffTime

instance Show TimeRange where
  show = showTimeRange

{-
TimeRange の終了時刻を求める。
-}
endTimeRange :: TimeRange -> ZonedTime
endTimeRange (TimeRange startTime seconds) = utcToZonedTime zone endUTC 
  where zone = zonedTimeZone startTime
        startUTC = zonedTimeToUTC startTime
        endUTC = addUTCTime seconds startUTC

showTimeRange :: TimeRange -> String
showTimeRange timeRange@(TimeRange startTime _) =
  "TimeRange " ++ (show startTime) ++ " ~ " ++ (show (endTimeRange timeRange))

{-
メッセージが自分のものか？
-}
isMyMessage :: String -> Message -> Bool
isMyMessage name message = case message of
  Message _ (User name' _) _ | name' == name -> True
  _ -> False

isMessage :: Message -> String -> Bool
isMessage message txt = case message of
  Message _ _ txt' -> txt == txt'

isIn :: Message -> Bool
isIn message = isMessage message "in"

isOut :: Message -> Bool
isOut message = isMessage message "out"

data MsgType = InMsg | OutMsg | OtherMsg

msgType :: Message -> MsgType
msgType message =
  case isIn message of
    True -> InMsg
    False -> case isOut message of
      True -> OutMsg
      False -> OtherMsg

messageTime :: Message -> ZonedTime
messageTime message = case message of
  Message zonedTime _ _ -> zonedTime

secondsBetween :: Message -> Message -> NominalDiffTime
secondsBetween msg1 msg2 = abs diff
  where utc1 = zonedTimeToUTC $ messageTime msg1
        utc2 = zonedTimeToUTC $ messageTime msg2
        diff = diffUTCTime utc1 utc2

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

buildTimeRange :: Message -> Message -> TimeRange
buildTimeRange inMsg outMsg = TimeRange startTime seconds
  where startTime = messageTime inMsg
        seconds = secondsBetween inMsg outMsg

{-
時刻範囲を取り出す
-}
extractTimeRange :: [Message] -> [TimeRange]
extractTimeRange msgs = reverse $ processMsg msgs Nothing Nothing Nothing []
  where
    -- msgs: 未処理メッセージ
    -- inMsg: in メッセージ
    -- firstMsg: 今期間最初のメッセージ
    -- lastMsg: 今期間最後のメッセージ
    processMsg :: [Message] -> Maybe Message -> Maybe Message -> Maybe Message -> [TimeRange] -> [TimeRange]
    processMsg msgs' inMsg firstMsg lastMsg ranges = case msgs' of
      x:xs -> case msgType x of
        InMsg -> case (inMsg, lastMsg) of
          (Just inMsg', Just lastMsg')
            | inMsg' == lastMsg' ->
              -- in が2度連続した場合は連続区間とみなす
              processMsg xs inMsg firstMsg (Just x) ranges
            | otherwise ->
              -- in が連続ではない場合、最後の発言までを前区間とみなす
              processMsg xs (Just x) (Just x) (Just x) (newRange:ranges)
              where newRange = buildTimeRange inMsg' lastMsg'
          (Nothing, _) -> processMsg xs (Just x) (Just x) (Just x) ranges
          (Just _, Nothing) -> error("should not be happened")
        OutMsg -> processMsg xs Nothing Nothing Nothing newRanges
          where maybeNewRange = case inMsg of
                  Just inMsg' -> Just $ buildTimeRange inMsg' x
                  Nothing -> case firstMsg of
                    Just firstMsg' -> Just $ buildTimeRange firstMsg' x
                    -- out だけがあった場合
                    Nothing -> Nothing
                newRanges = case maybeNewRange of
                  Just newRange -> newRange:ranges
                  Nothing -> ranges
        OtherMsg -> processMsg xs inMsg' firstMsg' (Just x) ranges
          where inMsg' = inMsg `orElse` Just x
                firstMsg' = firstMsg `orElse` Just x
      [] -> case (inMsg, lastMsg) of
        (Just inMsg', Just lastMsg') | inMsg' /= lastMsg' ->
          (buildTimeRange inMsg' lastMsg'):ranges
        _ -> ranges
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

main :: IO()
main =  do
  day <- getDayFromArg
  case day of
    Just d -> do
       messagess <- mapM fetchMessages $ getMonthDays d
       let
         timeRangess = map extractTimeRange messagess
         --normalss = map normalize timeRangess
       print timeRangess

    Nothing -> putStrLn "Invalid day. Pass year and month."
