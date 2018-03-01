module ModuleSystem where

import Syntax
import Aliases
import Simplifier
import Sorting
import Parser hiding (spec, modDecl, decls)
import TypeChecker
import Operators
import Qualification
import Data.Functor.Identity(runIdentity)
import Data.List(nub)
import Data.HashMap.Strict(insert)
import System.FilePath(takeDirectory, takeBaseName)
import Control.Monad.Trans.State.Strict(StateT(StateT), runStateT)

loadProgram path =
  do
    let dir = takeDirectory path
    let modName = fromString (takeBaseName path)

    modDecl <- loadModule dir modName
    mods <- growModuleEnv dir [modDecl]
    let simplified = pipeline mods
    typecheckProgram simplified

    return simplified

pipeline ::
    [ModuleDeclaration] ->
    [ModuleDeclaration]
pipeline = sortDeclsMod
    . aliasProgram
    . aliasOperatorsProgram
    . removeParens
    . fixAssocProgram
    . qualifyProgram
    . splitLambdas
    . removeFunctionDeclaration
    . sortModules

loadModule dir modName =
    let path = dir ++ "/" ++ toPath modName
    in do
        putStrLn ("Loading module " ++ pretty modName ++ " from " ++ path)
        parseFile path

{- Load all imported modules -}
growModuleEnv dir env =
  let
    imported = fmap getName env
    imports = foldMap gatherImports env
  in case nub (excluding imported imports) of
        [] -> return env
        needed -> do
            mods <- traverse (loadModule dir) needed
            growModuleEnv dir (mods ++ env)

{- Typechecking -}
typecheckProgram =
    feedbackM (ret filterNames) logEnv typecheckModule mempty

logEnv env modDecl =
    do
        let modName = getName modDecl
        writeFile ("logs/" ++ pretty modName ++ ".log") (prettyEnv env)

{- Operators and Aliasing -}
qualifyProgram =
    feedback filterIds qualifyNames (captureSimple captureNames) mempty

fixAssocProgram = performSimple fixAssoc captureAssocs

aliasOperatorsProgram =
    performSimple aliasOperators captureOperatorAliases

aliasProgram = performSimple aliasTypes captureAliases

performSimple action capture =
    feedback filterNames action (captureSimple capture) mempty

captureSimple capture importedTable modDecl =
    capture modDecl `mappend` importedTable

{-
Incrementally grow an environment and perform an action on all modules
capture : captures the environment e.g. the operators and their aliases
envs : a map of the module name and its captured environment
-}
feedbackM envFilter action capture envs mods =
    fmap fst (mapAccumM (step envFilter action capture) envs mods)

-- Convenience function to make a regular function monadic
ret f a b = return (f a b)

feedback envFilter action capture envs mods =
    runIdentity (feedbackM (ret envFilter) (ret action) (ret capture) envs mods)

step envFilter action capture envs modDecl =
    do
        let specs = gatherSpecs modDecl
        filtered <- envFilter specs envs
        captured <- capture filtered modDecl
        result <- action captured modDecl
        return (result, insert (getName modDecl) captured envs)

mapAccumM f s t = runStateT (traverse (StateT . flip f) t) s

filterIds imports envs =
    foldMap (\(modName, spec) ->
        case spec of
            Just ids -> includingKeys ids (find modName envs)
            Nothing -> mempty) imports

filterNames imports envs =
    foldMap (\(modName, _) -> find modName envs) imports
