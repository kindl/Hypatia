module Main(main) where

import Commands
import System.Environment


main =
  do
    args <- getArgs
    case args of
        ["compile", path] -> compileFromPath path
        _ -> putStrLn "Usage: hypatia compile path"
