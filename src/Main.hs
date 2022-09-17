{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.Foldable(traverse_, foldMap')
import Data.List(isPrefixOf)
import Syntax
import Compiler
import Transformations
import Parser hiding (modDecl)
import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashSet as Set

main = do
    args <- getArgs
    case fmap Text.pack args of
        ["compile", modName] -> compileProgram modName "lua" renderLua
        ["compiletojs", modName] -> compileProgram modName "js" renderJs
        _ -> putStrLn "Usage: hypatia compile A.Module.Name"

compileProgram modName abbreviation renderFun =
    if Text.isSuffixOf ".hyp" modName
        then putStrLn "Enter a module name instead of a path"
        else do
            program <- loadProgram (fromText modName)
            traverse_ (writeResult abbreviation renderFun) program

-- Load a list of modules from a module name
loadProgram modName = do
    loadedModule <- parseFromName modName
    mods <- growModuleEnv [loadedModule]
    simplified <- transformProgram mods
    _ <- typecheckProgram simplified
    return simplified

parseFromName modName = do
    let path = toPath modName
    putStrLn ("Parsing module " ++ renderName modName ++ " from " ++ path)
    m <- parseFile path
    if getName m == modName then return m else
        fail ("The file name did not match the name of the module " <> renderError (getName m))

-- Load all imported modules step-by-step
growModuleEnv env =
    let
        imported = Set.fromList (fmap getName env)
        imports = foldMap' importedModules env
        needed = Set.difference imports imported
    in if null needed
        then return env
        else do
                mods <- traverse parseFromName (Set.toList needed)
                growModuleEnv (mods ++ env)

writeResult abbreviation renderFun modDecl =
    let
        name = show (flatModName (getName modDecl))
        compiled = compile modDecl
    in if name == "Native" || name == "Main" || isPrefixOf "Native_" name
-- A module which name starts with "Native" is a native module by convention
-- meaning it is a module where the corresponding lua file is created by hand
-- Writing the compiled output is skipped to not override these files

-- Native modules can be used to call functions from Love 2D,
-- see examples/lua/Native.lua or Native_Love.lua

-- Main is also excluded to not override the file main.lua
-- which is used as an entry point by Love 2D
        then putStrLn ("Skipped writing native module " ++ name ++ "." ++ abbreviation)
        else Text.writeFile (abbreviation ++ "/" ++ name ++ "." ++ abbreviation) (renderFun compiled)
