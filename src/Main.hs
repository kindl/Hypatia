{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.Foldable(traverse_, foldMap')
import Syntax
import Compiler
import Transformations
import Parser(parseFile)
import System.Environment(getArgs)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashSet as Set


main = do
    args <- getArgs
    case args of
        ["compile", path] -> compileProgram path "lua" renderLua
        ["compiletojs", path] -> compileProgram path "js" renderJs
        _ -> putStrLn "Usage: hypatia compile Module.hyp"

compileProgram path abbreviation renderFun = do
    program <- loadProgram (normalizePath path)
    traverse_ (writeResult abbreviation renderFun) program

loadProgram path = do
    putStrLn ("Compiling module from " ++ path)
    loadedModule <- parseFile path
    mods <- growModuleEnv [loadedModule]
    program <- transformProgram mods
    _ <- typecheckProgram program
    return program

normalizePath =
    Text.unpack . dropPrefix "./" . Text.replace "\\" "/" . Text.pack

dropPrefix prefix t =
    if Text.isPrefixOf prefix t
        then Text.drop (Text.length prefix) t
        else t

parseFromName modName = do
    let path = toPath modName
    putStrLn ("Parsing module " ++ renderName modName ++ " from " ++ path)
    m <- parseFile path
    if getName m == modName
        then return m
        else fail ("The parsed module name " <> renderError (getName m)
            <> " did not match the expected module name " <> renderName modName
            <> " from path " <> show path)

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
        name = render (flatModName (getName modDecl))
        compiled = compile modDecl
    in if name == "Native" || name == "Main" || Text.isPrefixOf "Native_" name
-- A module which name starts with "Native" is a native module by convention
-- meaning it is a module where the corresponding lua file is created by hand
-- Writing the compiled output is skipped to not override these files

-- Native modules can be used to call functions from Love 2D,
-- see examples/lua/Native.lua or Native_Love.lua

-- Main is also excluded to not override the file main.lua
-- which is used as an entry point by Love 2D
        then putStrLn ("Skipped writing native module " ++ Text.unpack name ++ "." ++ abbreviation)
        else Text.writeFile (abbreviation ++ "/" ++ Text.unpack name ++ "." ++ abbreviation) (renderFun compiled)
