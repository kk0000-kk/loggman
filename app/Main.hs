{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.FilePath (takeExtension)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            putStrLn ">"
            input <- T.getLine
            T.writeFile filePath input
            putStrLn $ "Text has been written to " ++ filePath
        _ -> putStrLn "Usage: stack run <file-path>"
