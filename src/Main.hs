module Main(main) where

import ModuleSystem
import Transpiler
import Syntax
import System.Environment

compileFile path =
  do
    program <- loadProgram path
    mapM_ writeResult program

writeResult mod = do
    writeFile ("lua/" ++ pretty (getName mod) ++ ".lua") (renderLua (transpile mod))
    writeFile ("unityscript/" ++ pretty (getName mod) ++ ".js") (renderUnityScript (transpile mod))

main =
  do
    args <- getArgs
    case args of
        ["compile", path] -> compileFile path
        _ -> putStrLn "Usage: hypatia compile path"
