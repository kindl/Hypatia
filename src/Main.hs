module Main(main) where

import ModuleSystem
import Transpiler
import Syntax
import System.Environment

compileFile path =
  do
    program <- loadProgram path
    mapM_ (\mod -> writeFile ("lua/" ++ pretty (getName mod) ++ ".lua") (renderLua (transpile mod))) program

main =
  do
    args <- getArgs
    case args of
        ["compile", path] -> compileFile path
        _ -> putStrLn "Usage: hypatia compile path"
