module ModuleSystem where

import Syntax
import Aliases
import Simplifier
import Sorting
import Parser
import TypeChecker
import Operators
import Qualification
import Data.Functor.Identity(runIdentity)
import Data.List(nub)
import Data.HashMap.Strict(insert)
import System.FilePath(takeDirectory, takeBaseName)
import Control.Monad.Trans.State(StateT(StateT), runStateT)


loadProgram path =
  do
    let dir = takeDirectory path
    let modName = fromString (takeBaseName path)

    mod <- loadModuleWithSpec dir modName
    mods <- growModuleEnv dir [mod]
    let simplified = pipeline mods
    typechecking simplified

    return (fmap fst simplified)

pipeline ::
    [(ModuleDeclaration, [(Name, Maybe [Id])])] ->
    [(ModuleDeclaration, [(Name, Maybe [Id])])]
pipeline = sortDeclsMod . aliasProgram
    . aliasOperatorsProgram . removeParens
    . fixAssocProgram . qualifyProgram
    . simplifications . sortModules

loadModule dir modName =
    let path = dir ++ "/" ++ toPath modName
    in do
        putStrLn ("Loading module " ++ pretty modName ++ " from " ++ path)
        parseFile path

modulePair m =
    (m, gatherImports (getDecls m))

loadModuleWithSpec dir modName =
    fmap modulePair (loadModule dir modName)

{- Load all imported modules -}
growModuleEnv dir env =
  let
    imported = fmap (getName . fst) env
    imports = fmap fst (foldMap snd env)
  in case nub (excluding imported imports) of
        [] -> return env
        needed -> do
            mods <- traverse (loadModuleWithSpec dir) needed
            growModuleEnv dir (mods ++ env)

{- Typechecking -}
typechecking =
    feedbackM (ret filterNames) logEnv typecheckModule mempty

logEnv env mod =
    do
        let modName = getName mod
        writeFile ("logs/" ++ pretty modName ++ ".log") (prettyEnv env)
        return mod

{- Operators and Aliasing -}
qualifyProgram =
    feedback filterIds qualifyNames (captureSimple captureNames) mempty

fixAssocProgram = performSimple fixAssoc captureAssocs

aliasOperatorsProgram =
    performSimple aliasOperators captureOperatorAliases

aliasProgram = performSimple aliasTypes captureAliases

performSimple action capture =
    feedback filterNames action (captureSimple capture) mempty

captureSimple capture importedTable mod =
    capture mod `mappend` importedTable

{-
Incrementally grow an environment and perform an action on all modules
capture : captures the environment e.g. the operators and their aliases
envs : an association list of the module name and its captured environment
-}
feedbackM envFilter action capture envs mods =
    fmap fst (mapAccumM (step envFilter action capture) envs mods)

-- Convenience function to make a regular function monadic
ret f a b = return (f a b)

feedback envFilter action capture envs mods =
    runIdentity (feedbackM (ret envFilter) (ret action) (ret capture) envs mods)

step envFilter action capture envs (mod, imports) =
    do
        filtered <- envFilter imports envs
        captured <- capture filtered mod
        result <- action captured mod
        return ((result, imports), insert (getName mod) captured envs)

mapAccumM f s t = runStateT (traverse (StateT . flip f) t) s

filterIds imports envs =
    foldMap (\(modName, spec) ->
        case spec of
            Just ids -> includingKeys ids (find modName envs)
            Nothing -> mempty) imports

filterNames imports envs =
    foldMap (\(modName, _) -> find modName envs) imports

gatherImports decls = 
    [(name, spec) | ImportDeclaration name spec _ <- decls]
