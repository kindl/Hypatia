module ModuleSystem where

import Syntax
import Aliases
import Simplifier
import Sorting
import Parser hiding (spec, modDecl)
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
    . fmap aliasDecls
    . removeParens
    . fixAssocProgram
    . qualifyProgram
    . splitLambdas
    . removeFunctionDeclaration
    . sortModules

loadModule dir modName =
    let path = dir ++ "/" ++ toPath modName
    in do
        putStrLn ("Loading module "
            ++ renderName modName ++ " from " ++ path)
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
typecheckProgram = feedbackM logEnv (\envs modDecl ->
    typecheckModule (filterNames (gatherSpecs modDecl) envs) modDecl)

logEnv env modDecl =
    let
        modName = getName modDecl
        path = "logs/" ++ renderName modName ++ ".log"
    in writeFile path (renderEnv env)

{- Operators and Aliasing -}
qualifyProgram =
    feedback qualifyNames (captureSimple filterIds captureNames)

fixAssocProgram = feedbackSimple fixAssoc captureAssocs

aliasOperatorsProgram =
    feedbackSimple aliasOperators captureOperatorAliases

aliasProgram = feedbackSimple aliasTypes captureAliases

feedbackSimple action capture =
    feedback action (captureSimple filterNames capture)

captureSimple filterEnvs capture envs modDecl =
    capture modDecl `mappend` filterEnvs (gatherSpecs modDecl) envs

{-
Incrementally grow an environment
and perform an action on all modules

capture: captures the environment e.g. operators and their aliases
envs: a map of the module name and its captured environment
-}
feedbackM action capture mods =
    fmap fst (mapAccumM (step action capture) mempty mods)

step action capture envs modDecl =
    do
        captured <- capture envs modDecl
        result <- action captured modDecl
        return (result, insert (getName modDecl) captured envs)

feedback action capture mods =
    runIdentity (feedbackM (ret action) (ret capture) mods)

-- Convenience function to make a regular function monadic
ret f a b = return (f a b)

mapAccumM f s t = runStateT (traverse (StateT . flip f) t) s

filterIds imports envs =
    foldMap (\(modName, spec) ->
        case spec of
            Just ids -> includingKeys ids (find modName envs)
            Nothing -> mempty) imports

filterNames imports envs =
    foldMap (\(modName, _) -> find modName envs) imports
