module Main(main) where

import Data.Foldable(traverse_, foldMap')
import Data.List(isPrefixOf)
import Syntax
import Compiler
import Transformations
import Parser hiding (modDecl)
import System.Environment


main = do
    args <- getArgs
    case args of
        ["compile", path] -> compileFromPath path
        _ -> putStrLn "Usage: hypatia compile path"


compileFromPath path = do
    program <- loadProgram path
    traverse_ writeResult program

-- Load a list of modules from a file path
loadProgram path = do
    putStrLn ("Compiling module from " ++ path)
    loadedModule <- parseFile path
    mods <- growModuleEnv [loadedModule]
    simplified <- transformProgram mods
    _ <- typecheckProgram simplified
    return simplified

parseFromName modName = do
    let path = toPath modName
    putStrLn ("Loading module " ++ path)
    m <- parseFile path
    if getName m == modName then return m else
        fail ("The file name did not match the name of the module " ++ pretty (getName m))

-- Load all imported modules step-by-step
growModuleEnv env =
    let
        imported = fmap getName env
        imports = foldMap' gatherImports env
    in case excluding imported imports of
            [] -> return env
            needed -> do
                mods <- traverse parseFromName needed
                growModuleEnv (mods ++ env)

writeResult modDecl =
    let
        name = renderFlatModName (getName modDecl)
        compiled = compile modDecl
    in if name == "Native" || name == "Main" || isPrefixOf "Native_" name
-- A module which name starts with "Native" is a native module by convention
-- meaning it is a module where the corresponding lua file is created by hand
-- Writing the compiled output is skipped to not override these files

-- Native modules can be used to call functions from Love 2D,
-- see examples/lua/Native.lua or Native_Love.lua

-- Main is also excluded to not override the file main.lua
-- which is used as an entry point by Love 2D
        then putStrLn ("Skipped writing native module " ++ name ++ "(.lua)")
        else writeFile ("lua/" ++ name ++ ".lua") (renderLua compiled)
-- NOTE disabled javascript output
-- *> writeFile ("javascript/" ++ name ++ ".js") (renderJavaScript compiled)
