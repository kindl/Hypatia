module Transformations where

import Syntax
import Aliases
import Simplifier
import Sorting
import Typechecker
import Operators
import Qualification
import Data.Functor.Identity(runIdentity)
import Data.HashMap.Strict(insert)
import Control.Monad.Trans.State.Strict(StateT(StateT), runStateT)


-- The lowest function in this list is the first step of the transformations
transformations :: [ModuleDeclaration] -> [ModuleDeclaration]
transformations = sortDeclsMod
    -- Aliases
    . aliasConstructorsProgram
    . aliasOperatorsProgram
    -- Simplifier
    . removeParens
    -- Operators
    . fixAssocProgram
    -- Qualification
    . qualifyProgram
    . qualifyTypesProgram
    . fmap changeQualifiedImportsMod
    -- Simplifier
    . splitLambdas
    . removeAwaitDeclaration
    . removeFunctionDeclaration
    -- Aliases
    . fmap aliasOperatorsMod
    -- Sorting
    . sortModules

{- Typechecking -}
typecheckProgram p = feedbackM logEnv (\envs m ->
    typecheckModule (filterNames (gatherSpecs m) envs) m) p

logEnv env m =
    let
        modName = getName m
        path = "logs/" ++ renderName modName ++ ".log"
    in writeFile path (renderEnv env)

{- Operators and Aliasing -}
qualifyProgram =
    feedback qualifyNames (captureSimple filterIds captureNames)

qualifyTypesProgram =
    feedback qualifyTypeNames (captureSimple filterIds captureTypeNames)

fixAssocProgram =
    feedback fixAssoc (captureSimple filterNames captureAssocs)

aliasOperatorsProgram =
    feedback aliasOperators (captureSimple filterNames captureOperatorAliases)

aliasConstructorsProgram =
    feedback aliasConstructors (captureSimple filterNames captureAliases)

captureSimple filterEnvs capture envs m =
    capture m `mappend` filterEnvs (gatherSpecs m) envs

{-
Incrementally grow an environment
and perform an action on all modules

capture: captures the environment e.g. operators and their aliases
envs: a map of the module name and its captured environment
-}
feedbackM action capture mods =
    fmap fst (mapAccumM (step action capture) mempty mods)

step action capture envs m = do
    captured <- capture envs m
    result <- action captured m
    return (result, insert (getName m) captured envs)

feedback action capture mods =
    runIdentity (feedbackM (ret action) (ret capture) mods)

-- Convenience function to make a regular function monadic
ret f a b = return (f a b)

mapAccumM f s t = runStateT (traverse (StateT . flip f) t) s

filterIds imports envs =
    foldMap (\(modName, importedIds) ->
        case importedIds of
            Just ids -> includingKeys ids (find modName envs)
            Nothing -> mempty) imports

filterNames imports envs =
    foldMap (\(modName, _) -> find modName envs) imports
