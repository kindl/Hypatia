module Main(main) where

import Data.Foldable(traverse_, foldMap')
import Data.Maybe(catMaybes)
import Syntax
import Compiler
import Transformations
import Parser(parseFile)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import System.FilePath(takeDirectory)
import System.Directory(createDirectoryIfMissing, copyFile,
    doesFileExist, doesDirectoryExist, listDirectory)
import Options.Applicative


data Options = Options {
    getAbbreviation :: String,
    getLibDirs :: [FilePath],
    getPath :: FilePath
}

opts = info (commands <**> helper) mempty

commands = hsubparser (
    command "compile" (info (compileCommand "lua") (progDesc "Compile to lua"))
    <> command "compiletojs" (info (compileCommand "js") (progDesc "Compile to js"))
    )

libDirOption =
    strOption (
        long "library"
        <> help "Directory for standard library"
        <> metavar "FILEPATH")

compileCommand abbreviation =
    Options abbreviation
        <$> many libDirOption
        <*> argument str (metavar "FILEPATH")

main = do
    options <- execParser opts
    compileProgram options

compileProgram options =
    let
        abbreviation = options.getAbbreviation
        libraryDirectories = options.getLibDirs
        path = options.getPath
        normalizedPath = normalizePath path
        baseDir = takeDirectory normalizedPath
        buildDir = "build/" ++ abbreviation
    in do
        program <- loadProgram libraryDirectories baseDir normalizedPath
        traverse_ (copyAssetDirectory abbreviation buildDir) (baseDir:libraryDirectories) 
        writeProgram buildDir abbreviation program

copyAssetDirectory abbreviation buildDir folder =
    let assetDir = folder ++ "/" ++ abbreviation
    in copyDirectoryIfExists assetDir buildDir

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

loadProgram libraryDirectories baseDir path = do
    putStrLn ("Compiling module from \"" ++ path ++ "\" using libraries " ++ show libraryDirectories)
    loadedModule <- parseFile path
    mods <- growModuleEnv libraryDirectories baseDir [loadedModule]
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
-- then looks for the file in the library directories
-- TODO: this is not fully sensible behavior.
-- A library should look for its dependencies where it is located first.
parseFromName libraryDirectories baseDir modName =
    let
        modPath = toPath modName
        pathOptions = fmap (\dir -> dir ++ "/" ++ modPath) (baseDir:libraryDirectories)
    in do
        existing <- traverse maybeExists pathOptions
        case catMaybes existing of
            [] -> fail ("Error: Module " <> renderName modName <> " not found")
            [path] -> parseFileWithInfo modName path
            paths@(path:_) -> do
                putStrLn ("Warning: Module " <> renderName modName <> " has several options " <> show paths)
                parseFileWithInfo modName path

maybeExists path = do
    exists <- doesFileExist path
    return (if exists
        then Just path
        else Nothing)

parseFileWithInfo modName path = do
    putStrLn ("Parsing module " ++ renderName modName ++ " from " ++ path)
    modDecl <- parseFile path
    if modDecl.getName == modName
        then return modDecl
        else fail ("The parsed module name " <> renderError modDecl.getName
            <> " did not match the expected module name " <> renderName modName
            <> " from path " <> show path)

-- Load all imported modules step-by-step
growModuleEnv libraryDirectories baseDir env =
    let
        imported = Set.fromList (fmap (.getName) env)
        imports = foldMap' importedModules env
        needed = Set.difference imports imported
    in if null needed
        then return env
        else do
                mods <- traverse (parseFromName libraryDirectories baseDir) (Set.toList needed)
                growModuleEnv libraryDirectories baseDir (mods ++ env)

writeProgram buildDir abbreviation =
    feedbackM (simpleActionA (writeResult buildDir abbreviation) filterNames (return . captureArity))

getRender "lua" = renderLua
getRender "js" = renderJs
getRender other =
    error ("No rendering function for" ++ show other)

getKeywordSet "lua" = luaKeywords
getKeywordSet "js" = jsKeywords
getKeywordSet other =
    error ("No keyword set for " ++ show other)

writeResult buildDir abbreviation arityMap modDecl =
    let
        name = render (flatModName modDecl.getName)
        compiled = compile modDecl
        fileName = Text.unpack name ++ "." ++ abbreviation
        filePath = buildDir ++ "/" ++ fileName
        keywords = getKeywordSet abbreviation
        renderFun = getRender abbreviation
        optimized = optimize arityMap keywords compiled
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
            let message = if doesExist then " exists at " else " does NOT exist at "
            putStrLn ("Native module " ++ fileName ++ message ++ filePath)
        else
            Text.writeFile filePath (renderFun optimized)
