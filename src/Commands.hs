module Commands where

import Data.Foldable(traverse_)
import Data.List(isPrefixOf)
import Syntax
import Compiler
import ModuleSystem
import Parser hiding (modDecl)


compileFromPath path =
  do
    program <- loadProgram path
    traverse_ writeResult program


-- Load a list of modules from a file path
loadProgram path = do
    loadedModule <- loadFromPath path
    mods <- growModuleEnv [loadedModule]
    let simplified = transformations mods
    _ <- typecheckProgram simplified
    return simplified

loadFromName modName = do
    m <- loadFromPath (toPath modName)
    if getName m == modName then return m else
        fail ("The file name did not match the name of the module " ++ pretty (getName m))

loadFromPath path = do
    putStrLn ("Loading module from " ++ path)
    parseFile path

-- Load all imported modules step-by-step
growModuleEnv env =
    let
        imported = fmap getName env
        imports = foldMap gatherImports env
    in case excluding imported imports of
            [] -> return env
            needed -> do
                mods <- traverse loadFromName needed
                growModuleEnv (mods ++ env)


writeResult modDecl =
    let
        name = renderFlatModName (getName modDecl)
        compiled = compile modDecl
    in if name == "Native" || name == "Main" || isPrefixOf "Native_" name
-- NOTE the Native module is not written
-- If there are more than one Native module
-- then others could be added with a flag
        then putStrLn ("Skipped writing native module " ++ name ++ "(.lua)")
        else writeFile ("lua/" ++ name ++ ".lua") (renderLua compiled)
-- NOTE disabled javascript output
-- *> writeFile ("javascript/" ++ name ++ ".js") (renderJavaScript compiled)
