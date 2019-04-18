module Main(main) where

import ModuleSystem
import Compiler
import Syntax
import System.Environment
import Data.Foldable(traverse_)
import Data.List(isPrefixOf)

compileFile path =
  do
    program <- loadProgram path
    traverse_ writeResult program

writeResult modDecl =
    let
        name = renderFlatModName (getName modDecl)
        compiled = compile modDecl
    in if name == "Native" || isPrefixOf "Native_" name
-- NOTE the Native module is not written
-- If there are more than one Native module
-- then others could be added with a flag
        then putStrLn ("Skipped writing native module " ++ name ++ "(.lua)")
        else writeFile ("lua/" ++ name ++ ".lua") (renderLua compiled)
-- NOTE disabled javascript output
-- *> writeFile ("javascript/" ++ name ++ ".js") (renderJavaScript compiled)

main =
  do
    args <- getArgs
    case args of
        ["compile", path] -> compileFile path
        _ -> putStrLn "Usage: hypatia compile path"
