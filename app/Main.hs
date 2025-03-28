{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import Data.Text (Text, append, pack)
import qualified Data.Text.IO as T
import System.FilePath (takeExtension)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> loop filePath
        _ -> putStrLn "Usage: stack run <file-path>"

loop :: FilePath -> IO ()
loop filePath = do
    putStr "> "
    hFlush stdout
    input <- T.getLine
    if input == "exit"
        then putStrLn "Exiting..."
        else do
            T.appendFile filePath ("\n")
            currentTime <- getZonedTime
            let timeStamp = formatTime defaultTimeLocale "%Y-%m-%d(%a) %H:%M:%S" currentTime
            T.appendFile filePath (pack timeStamp `append` "\n")
            T.appendFile filePath (input `append` "\n")
            putStrLn $ "Text has been written to " ++ filePath
            loop filePath
