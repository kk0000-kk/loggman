{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import Data.Text (Text, append, pack, replace, isPrefixOf, lines, stripPrefix)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HM
import System.FilePath (takeExtension)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Process (callCommand, system)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import ConfigLoader

defaultConfig :: Config
defaultConfig = Config
    { toggl = TogglConfig
        { apiKey = "dummy"
        , projectIds = HM.fromList [("dummy-project", "dummy-id")]
        }
    }

main :: IO ()
main = do
    args <- getArgs
    homeDir <- getHomeDirectory
    let configDir = homeDir ++ "/.loggman"
    let configPath = configDir ++ "/config.yaml"

    configResult <- loadConfig configPath
    config <- case configResult of
                Left err -> do
                    putStrLn $ "If you need, please write the configuration in ~/.loggman/config.yaml." ++ show err
                    return defaultConfig
                Right cfg -> do
                    putStrLn $ "Configuration file loaded."
                    return cfg

    case args of
        [filePath] -> do
            fileExists <- doesFileExist filePath
            if not fileExists
                then do
                    -- ファイルが存在しない場合、テンプレートを書き込む
                    T.writeFile filePath "## TODO\n\n## LOG\n"
                    putStrLn $ "Template written to " ++ filePath
            else return ()
            runInputT defaultSettings (loop filePath)
        _ -> putStrLn "Usage: stack run <file-path>"

loop :: FilePath -> InputT IO ()
loop filePath = do
    outputStrLn "\n====================="
    outputStrLn "Current file content:\n"
    liftIO $ callCommand $ "cat " ++ filePath
    outputStrLn ""
    input <- getInputLine "> "
    case input of
        Nothing -> return ()
        Just "exit" -> do
            liftIO $ do
                T.appendFile filePath ("\n")
                timeStamp <- getCurrentTimeStamp
                T.appendFile filePath (timeStamp `append` "\n")
                T.appendFile filePath ("exit" `append` "\n")
            outputStrLn "Exiting..."
        Just "todo" -> do
            liftIO $ do
                T.appendFile filePath ("\n")
                timeStamp <- getCurrentTimeStamp
                T.appendFile filePath (timeStamp `append` "\n")
                T.appendFile filePath ("タスクばらし/start" `append` "\n")
            liftIO $ editMode filePath
            loop filePath
        Just inputText | "done" `isPrefixOf` pack inputText -> do
            let prefix = T.drop 5 (pack inputText)  -- "done "の後の部分を取得
            liftIO $ markTodoAsDone filePath prefix
            loop filePath
        Just "" -> loop filePath
        Just inputText -> do
            liftIO $ do
                T.appendFile filePath ("\n")
                timeStamp <- getCurrentTimeStamp
                T.appendFile filePath (timeStamp `append` "\n")
                T.appendFile filePath (pack inputText `append` "\n")
                putStrLn $ "Text has been written to " ++ filePath
            loop filePath

editMode :: FilePath -> IO ()
editMode filePath = do
    putStrLn "Entered edit mode. Opening vi editor..."
    _ <- system $ "vi " ++ filePath
    putStrLn "Exited edit mode."
    T.appendFile filePath ("\n")
    timeStamp <- getCurrentTimeStamp
    T.appendFile filePath (timeStamp `append` "\n")
    T.appendFile filePath ("タスクばらし/stop" `append` "\n")

markTodoAsDone :: FilePath -> Text -> IO ()
markTodoAsDone filePath prefix = do
    content <- T.readFile filePath
    let updatedContent = T.unlines $ map (markIfPrefix prefix) (T.lines content)
    T.writeFile filePath updatedContent
    T.appendFile filePath ("\n")
    timeStamp <- getCurrentTimeStamp
    T.appendFile filePath (timeStamp `append` "\n")
    T.appendFile filePath (prefix `append` "/done\n")
    putStrLn "Marked TODOs as done."

markIfPrefix :: Text -> Text -> Text
markIfPrefix prefix line = case stripPrefix "- [ ] " strippedLine of
    Just strippedPrefix | prefix `isPrefixOf` strippedPrefix -> replace "- [ ]" "- [x]" line
    _ -> line
  where
    strippedLine = T.dropWhile (== ' ') line

getCurrentTimeStamp :: IO Text
getCurrentTimeStamp = do
    currentTime <- getZonedTime
    let timeStamp = formatTime defaultTimeLocale "%Y-%m-%d(%a) %H:%M:%S" currentTime
    return $ pack timeStamp
