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

writeResult modDecl =
    writeFile ("lua/" ++ pretty (getName modDecl) ++ ".lua") (renderLua (compile modDecl))
--    *> writeFile ("javascript/" ++ pretty (getName modDecl) ++ ".js") (renderJavaScript (compile modDecl))

main =
  do
    args <- getArgs
    case args of
        ["compile", path] -> compileFile path
        _ -> putStrLn "Usage: hypatia compile path"
