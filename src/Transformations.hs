module Transformations where

import Syntax
import Aliases
import Simplifier
import Sorting
import Typechecker
import Operators
import Qualification
import Data.HashMap.Strict(insert, (!))
import Data.Foldable(foldMap')
import Control.Monad.Trans.State.Strict(StateT(StateT), runStateT)
import Control.Monad((<=<))


transformProgram ms = either fail return (transformations ms)

-- The lowest function in this list is the first step of the transformations
transformations = traverse sortDeclsMod
    -- Aliases
    <=< aliasConstructorsProgram
    <=< aliasOperatorsProgram
    -- Simplifier
    <$> fmap removeParens
    -- Operators
    <=< fixAssocProgram
    -- Qualification
    <$> fmap qualifyAliases
    <=< qualifyProgram
    <=< qualifyTypesProgram
    <$> fmap changeQualifiedImportsMod
    -- Simplifier
    <$> fmap simplifications
    -- Aliases
    <=< traverse aliasOperatorsMod
    -- Sorting
    <=< sortModules

{- Typechecking -}
typecheckProgram p = feedbackM typecheckAction p
    where typecheckAction envs m = do
            let filtered = filterNames (gatherSpecs m) envs
            captured <- typecheckModule filtered m
            let path = "logs/" ++ renderName (getName m) ++ ".log"
            writeFile path (renderEnv captured)
            return ((), captured)

{- Operators and Aliasing -}
qualifyProgram =
    feedbackM (simpleActionA qualifyNames filterIds captureNames)

qualifyTypesProgram =
    feedbackM (simpleActionA qualifyTypeNames filterIds captureTypeNames)

fixAssocProgram =
    feedbackM (simpleActionA fixAssoc filterNames captureAssocs)

aliasOperatorsProgram =
    feedbackM (simpleActionA aliasOperators filterNames captureOperatorAliases)

aliasConstructorsProgram =
    feedbackM (simpleActionA aliasConstructors filterNames captureAliases)

-- Run action with captured local env and imported envs
-- but return only the local environment
simpleActionA action filterEnvs capture envs m =
    let
        captured = capture m
        combined = captured `mappend` filterEnvs (gatherSpecs m) envs
    in fmap (\result -> (result, captured)) (action combined m)

-- Incrementally grow an environment and perform an action on all modules.
-- An action is a function that returns captured information e.g. operators and their aliases
-- and a changed module e.g. where operators have been changed to function application.
feedbackM action mods = fmap fst (mapAccumM step mempty mods)
    -- step could also be written without do-notation:
    --fmap (fmap (flip (insert (getName m)) envs)) (action envs m)
    where step envs m = do
            (result, captured) <- action envs m
            return (result, insert (getName m) captured envs)

mapAccumM f s t = runStateT (traverse (StateT . flip f) t) s

filterIds imports envs =
    foldMap' (\(modName, importedIds) ->
        case importedIds of
            Just ids -> includingKeys ids (envs ! modName)
            Nothing -> mempty) imports

filterNames imports envs =
    foldMap' (\(modName, _) -> envs ! modName) imports
