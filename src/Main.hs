module Main(main) where

import ModuleSystem
import Compiler
import Syntax
import System.Environment
import Data.Foldable(traverse_)

compileFile path =
  do
    program <- loadProgram path
    traverse_ writeResult program

writeResult mod = do
    writeFile ("lua/" ++ pretty (getName mod) ++ ".lua") (renderLua (compile mod))
    writeFile ("unityscript/" ++ pretty (getName mod) ++ ".js") (renderUnityScript (compile mod))

main =
  do
    args <- getArgs
    case args of
        ["compile", path] -> compileFile path
        _ -> putStrLn "Usage: hypatia compile path"
