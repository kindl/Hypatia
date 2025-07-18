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
    doesFileExist, doesDirectoryExist, listDirectory)


main = do
    args <- getArgs
    case args of
        ["compile", path] -> compileProgram path "lua"
        ["compiletojs", path] -> compileProgram path "js"
        _ -> putStrLn "Usage: hypatia compile Module.hyp"

compileProgram path abbreviation = do
    let normalizedPath = normalizePath path
    let libDir = "library"
    let baseDir = takeDirectory normalizedPath
    program <- loadProgram libDir baseDir normalizedPath
    let libraryAssetDir = libDir ++ "/" ++ abbreviation
    let localAssetDir = baseDir ++ "/" ++ abbreviation
    let buildDir = "build/" ++ abbreviation
    copyDirectoryIfExists libraryAssetDir buildDir
    copyDirectoryIfExists localAssetDir buildDir
    _ <- writeProgram buildDir abbreviation program
    return ()

copyDirectory srcDir dstDir = do
    createDirectoryIfMissing True dstDir
    contents <- listDirectory srcDir
    traverse_ (copyEntry srcDir dstDir) contents

copyDirectoryIfExists srcDir dstDir = do
    isDirectory <- doesDirectoryExist srcDir
    if isDirectory
        then putStrLn ("Copying directory " <> srcDir) >> copyDirectory srcDir dstDir
        else putStrLn ("Skipped copying directory " <> srcDir)

copyEntry srcDir dstDir name =
    let
        srcPath = srcDir ++ "/" ++ name
        dstPath = dstDir ++ "/" ++ name
    in do
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
            then copyDirectory srcPath dstPath
            else copyFile srcPath dstPath

loadProgram libDir baseDir path = do
    putStrLn ("Compiling module from " ++ path)
    loadedModule <- parseFile path
    mods <- growModuleEnv libDir baseDir [loadedModule]
    program <- transformProgram mods
    _ <- typecheckProgram program
    return program

normalizePath =
    Text.unpack . dropPrefix "./" . Text.replace "\\" "/" . Text.pack

dropPrefix prefix t =
    if Text.isPrefixOf prefix t
        then Text.drop (Text.length prefix) t
        else t

-- Turns a module name into a path
-- Tries to find the module locally first and
-- then looks for the file in the library directory
parseFromName libDir baseDir modName =
    let
        modPath = toPath modName
        localPath = baseDir ++ "/" ++ modPath
        libraryPath = libDir ++ "/" ++ modPath
    in do
        existsLocally <- doesFileExist localPath
        if existsLocally
            then parseFile' modName localPath
            else parseFile' modName libraryPath

parseFile' modName path = do
    putStrLn ("Parsing module " ++ renderName modName ++ " from " ++ path)
    m <- parseFile path
    if getName m == modName
        then return m
        else fail ("The parsed module name " <> renderError (getName m)
            <> " did not match the expected module name " <> renderName modName
            <> " from path " <> show path)

-- Load all imported modules step-by-step
growModuleEnv libDir baseDir env =
    let
        imported = Set.fromList (fmap getName env)
        imports = foldMap' importedModules env
        needed = Set.difference imports imported
    in if null needed
        then return env
        else do
                mods <- traverse (parseFromName libDir baseDir) (Set.toList needed)
                growModuleEnv libDir baseDir (mods ++ env)

writeProgram buildDir abbreviation =
    feedbackM (simpleActionA (writeResult buildDir abbreviation) filterNames (return . captureArity))

writeResult buildDir abbreviation arityMap modDecl =
    let
        name = render (flatModName (getName modDecl))
        compiled = compile modDecl
        fileName = Text.unpack name ++ "." ++ abbreviation
        filePath = buildDir ++ "/" ++ fileName
        keywords = if abbreviation == "js" then jsKeywords else luaKeywords
        renderFun = if abbreviation == "js" then renderJs else renderLua
        optimized = optimizeNames arityMap keywords compiled
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
            Text.writeFile filePath (renderFun optimized)
