{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import Data.Text (Text, append)
import qualified Data.Text.IO as T
import System.FilePath (takeExtension)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> loop filePath
        _ -> putStrLn "Usage: stack run <file-path>"

loop :: FilePath -> IO ()
loop filePath = do
    putStrLn "> "
    input <- T.getLine
    if input == "exit"
        then putStrLn "Exiting..."
        else do
            T.appendFile filePath (input `append` "\n")
            putStrLn $ "Text has been written to " ++ filePath
            loop filePath
