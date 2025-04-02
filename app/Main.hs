{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import Data.Text (Text, append, pack, replace, isPrefixOf, lines, stripPrefix)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (takeExtension)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import System.Directory (doesFileExist)
import System.Process (callCommand, system)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            fileExists <- doesFileExist filePath
            if not fileExists
              then do
                  -- ファイルが存在しない場合、テンプレートを書き込む
                  T.writeFile filePath "## TODO\n\n## LOG\n"
                  putStrLn $ "Template written to " ++ filePath
            else return ()
            loop filePath
        _ -> putStrLn "Usage: stack run <file-path>"

loop :: FilePath -> IO ()
loop filePath = do
    putStrLn "\n====================="
    putStrLn "Current file content:\n"
    callCommand $ "cat " ++ filePath
    putStrLn ""
    putStr "> "
    hFlush stdout  -- これにより、メッセージが即座に表示される
    input <- T.getLine
    if input == "exit"
        then do
            T.appendFile filePath ("\n")
            timeStamp <- getCurrentTimeStamp
            T.appendFile filePath (timeStamp `append` "\n")
            T.appendFile filePath ("exit" `append` "\n")
            putStrLn "Exiting..."
    else if input == "todo"
        then do
            T.appendFile filePath ("\n")
            timeStamp <- getCurrentTimeStamp
            T.appendFile filePath (timeStamp `append` "\n")
            T.appendFile filePath ("タスクばらし/start" `append` "\n")

            editMode filePath
    else if "done" `T.isPrefixOf` input
        then do
            let prefix = T.drop 5 input  -- "done "の後の部分を取得
            markTodoAsDone filePath prefix
    else if input == ""
        then loop filePath
    else do
        T.appendFile filePath ("\n")
        timeStamp <- getCurrentTimeStamp
        T.appendFile filePath (timeStamp `append` "\n")
        T.appendFile filePath (input `append` "\n")
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
    loop filePath

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
    loop filePath

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
