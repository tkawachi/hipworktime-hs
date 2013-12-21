module HipWorktime.History
       (
         User(User),
         Message(Message),
         fetchHistory,
       ) where

import Data.Time.Calendar
import Data.Time.LocalTime

-- ユーザ (id, name)
data User = User String String deriving Show

data Message = Message LocalTime User String deriving Show

{-
特定日の履歴一覧を得る。
-}
fetchHistory :: String -> String -> Day -> TimeZone -> IO [Message]
fetchHistory token room day zone = undefined
