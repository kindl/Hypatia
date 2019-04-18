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
import Data.HashMap.Strict(insert)
import Control.Monad.Trans.State.Strict(StateT(StateT), runStateT)

loadProgram path = do
    modDecl <- loadPath path
    mods <- growModuleEnv [modDecl]
    let simplified = pipeline mods
    typecheckProgram simplified
    return simplified

pipeline :: [ModuleDeclaration] -> [ModuleDeclaration]
pipeline = sortDeclsMod
    . aliasProgram
    . aliasOperatorsProgram
    . removeParens
    . fixAssocProgram
    . qualifyProgram
    . qualifyTypesProgram
    . fmap resolveQualifiedImports
    . splitLambdas
    . removeFunctionDeclaration
    . fmap aliasDecls
    . sortModules

loadModule modName = do
    m <- loadPath (toPath modName)
    if getName m == modName then return m else
        fail ("The file name did not match the name of the module " ++ pretty (getName m))

loadPath path = do
    putStrLn ("Loading module from " ++ path)
    parseFile path

{- Load all imported modules -}
growModuleEnv env =
  let
    imported = fmap getName env
    imports = foldMap gatherImports env
  in case excluding imported imports of
        [] -> return env
        needed:_ -> do
            modDecl <- loadModule needed
            growModuleEnv (modDecl:env)

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

qualifyTypesProgram =
    feedback qualifyTypeNames (captureSimple filterIds captureTypeNames)

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
