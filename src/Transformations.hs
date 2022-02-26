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
import Data.Foldable(foldMap')
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
typecheckProgram p = feedbackM typecheckAction p
    where typecheckAction envs m = do
            let filtered = filterNames (gatherSpecs m) envs
            captured <- typecheckModule filtered m
            let path = "logs/" ++ renderName (getName m) ++ ".log"
            writeFile path (renderEnv captured)
            return ((), captured)

{- Operators and Aliasing -}
qualifyProgram =
    feedback (simpleAction qualifyNames filterIds captureNames)

qualifyTypesProgram =
    feedback (simpleAction qualifyTypeNames filterIds captureTypeNames)

fixAssocProgram =
    feedback (simpleAction fixAssoc filterNames captureAssocs)

aliasOperatorsProgram =
    feedback (simpleAction aliasOperators filterNames captureOperatorAliases)

aliasConstructorsProgram =
    feedback (simpleAction aliasConstructors filterNames captureAliases)

-- Run action with captured local env and imported envs
-- but return only the local environment
simpleAction action filterEnvs capture envs m =
    let
        captured = capture m
        combined = captured `mappend` filterEnvs (gatherSpecs m) envs
        result = action combined m
    in (result, captured)

-- Incrementally grow an environment and perform an action on all modules.
-- An action is a function that returns captured information e.g. operators and their aliases
-- and a changed module e.g. where operators have been changed to function application.
feedbackM action mods = fmap fst (mapAccumM step mempty mods)
    -- step could also be written without do-notation:
    --fmap (fmap (flip (insert (getName m)) envs)) (action envs m)
    where step envs m = do
            (result, captured) <- action envs m
            return (result, insert (getName m) captured envs)

feedback action mods =
    runIdentity (feedbackM (\envs m -> return (action envs m)) mods)

mapAccumM f s t = runStateT (traverse (StateT . flip f) t) s

filterIds imports envs =
    foldMap' (\(modName, importedIds) ->
        case importedIds of
            Just ids -> includingKeys ids (find modName envs)
            Nothing -> mempty) imports

filterNames imports envs =
    foldMap' (\(modName, _) -> find modName envs) imports
