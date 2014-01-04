module HipWorktime.Config
  (
  readConfig
  ) where

import System.Directory

{-
設定ファイルのパス。
-}
getConfigPath :: IO FilePath
getConfigPath = do
  home <- getHomeDirectory
  return $ home ++ "/.hipworktimerc"

{-
ファイルから設定を読み込む。

設定ファイルは二行で、各行に以下の内容を書いておく。
* 認証token
* room id
* 自分の user id
-}
readConfigFile :: FilePath -> IO (String, String, String)
readConfigFile path = do
  contents <- readFile path
  let l = lines contents in return $ (l!!0, l!!1, l!!2)

readConfig :: IO (String, String, String)
readConfig = do
  path <- getConfigPath
  readConfigFile path
