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
import System.FilePath(takeDirectory)
import System.Directory(createDirectoryIfMissing, copyFile,
    doesDirectoryExist, listDirectory, doesFileExist)


main = do
    args <- getArgs
    case args of
        ["compile", path] -> compileProgram path "lua" renderLua
        ["compiletojs", path] -> compileProgram path "js" renderJs
        _ -> putStrLn "Usage: hypatia compile Module.hyp"

compileProgram path abbreviation renderFun = do
    let baseDir = takeDirectory path
    program <- loadProgram baseDir (normalizePath path)
    let assetDir = baseDir ++ "/" ++ abbreviation
    let buildDir = "build/" ++ abbreviation
    copyDirectory assetDir buildDir
    traverse_ (writeResult buildDir abbreviation renderFun) program

copyDirectory srcDir dstDir = do
    createDirectoryIfMissing True dstDir
    contents <- listDirectory srcDir
    traverse_ (copyEntry srcDir dstDir) contents

copyEntry srcDir dstDir name =
    let
        srcPath = srcDir ++ "/" ++ name
        dstPath = dstDir ++ "/" ++ name
    in do
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
            then copyDirectory srcPath dstPath
            else copyFile srcPath dstPath

loadProgram baseDir path = do
    putStrLn ("Compiling module from " ++ path)
    loadedModule <- parseFile path
    mods <- growModuleEnv baseDir [loadedModule]
    program <- transformProgram mods
    _ <- typecheckProgram program
    return program

normalizePath =
    Text.unpack . dropPrefix "./" . Text.replace "\\" "/" . Text.pack

dropPrefix prefix t =
    if Text.isPrefixOf prefix t
        then Text.drop (Text.length prefix) t
        else t

parseFromName baseDir modName = do
    let path = baseDir ++ "/" ++ toPath modName
    putStrLn ("Parsing module " ++ renderName modName ++ " from " ++ path)
    m <- parseFile path
    if getName m == modName
        then return m
        else fail ("The parsed module name " <> renderError (getName m)
            <> " did not match the expected module name " <> renderName modName
            <> " from path " <> show path)

-- Load all imported modules step-by-step
growModuleEnv baseDir env =
    let
        imported = Set.fromList (fmap getName env)
        imports = foldMap' importedModules env
        needed = Set.difference imports imported
    in if null needed
        then return env
        else do
                mods <- traverse (parseFromName baseDir) (Set.toList needed)
                growModuleEnv baseDir (mods ++ env)

writeResult buildDir abbreviation renderFun modDecl =
    let
        name = render (flatModName (getName modDecl))
        compiled = compile modDecl
        fileName = Text.unpack name ++ "." ++ abbreviation
        filePath = buildDir ++ "/" ++ fileName
    in if name == "Native" || name == "Main" || Text.isPrefixOf "Native_" name
-- A module which name starts with "Native" is a native module by convention
-- meaning it is a module where the corresponding lua file is created by hand
-- Writing the compiled output is skipped to not override these files

-- Native modules can be used to call functions from Love 2D,
-- see examples/lua/Native.lua or Native_Love.lua

-- Main is also excluded to not override the file main.lua
-- which is used as an entry point by Love 2D
        then do
            doesExist <- doesFileExist filePath
            putStrLn ("Native module " ++ fileName ++ " exists: " ++ show doesExist)
        else
            Text.writeFile filePath (renderFun compiled)
