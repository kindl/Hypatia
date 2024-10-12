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
import qualified Data.Text.IO as Text


transformProgram ms = either fail return (transformations ms)

-- The lowest function in this list is the first step of the transformations
transformations =
    traverse sortDeclsMod
    -- Annotations
    <=< annotateTagInfoProgram
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
    -- Aliases
    <=< traverse aliasOperatorsMod
    -- Simplifier
    <$> fmap simplifications
    -- Sorting
    <=< sortModules

{- Typechecking -}
typecheckProgram p = feedbackM typecheckAction p

typecheckAction envs m = do
    let filtered = filterNames (getImports m) envs
    captured <- typecheckModule filtered m
    let logPath = "logs/" ++ renderName (getName m) ++ ".log"
    Text.writeFile logPath (renderEnv captured)
    return ((), captured)

{- Operators and Aliasing -}
qualifyProgram =
    feedbackM (simpleActionA qualifyNames filterIds captureNames)

qualifyTypesProgram =
    feedbackM (simpleActionA qualifyTypeNames filterIds captureTypeNames)

fixAssocProgram =
    feedbackM (simpleActionA fixAssoc filterNames (return . captureAssocs))

aliasOperatorsProgram =
    feedbackM (simpleActionA aliasOperators filterNames (return . captureOperatorAliases))

aliasConstructorsProgram =
    feedbackM (simpleActionA aliasConstructors filterNames (return . captureAliases))

annotateTagInfoProgram =
    feedbackM (simpleActionA (\env m -> return (annotateTagInfo env m)) filterNames (return . captureTagInfo))

-- Run action with captured local env and imported envs
-- but return only the local environment
simpleActionA action filterEnvs capture envs m = do
    captured <- capture m
    let combined = captured `mappend` filterEnvs (getImports m) envs
    result <- action combined m
    pure (result, captured)

-- Incrementally grow an environment and perform an action on all modules.
-- An action is a function that returns captured information e.g. operators and their aliases
-- and a changed module e.g. where operators have been changed to function application.
feedbackM action mods = fmap fst (mapAccumM step mempty mods)
    -- step could also be written without do-notation:
    --fmap (fmap (flip (insert (getName m)) envs)) (action envs m)
    where step envs m = do
            (result, captured) <- action envs m
            return (result, insert (getName m) captured envs)

-- TODO Remove with base-4.18
mapAccumM f s t = runStateT (traverse (StateT . flip f) t) s

filterIds imports envs =
    foldMap' (\(ImportDeclaration modName importedIds _) ->
        case importedIds of
            Just ids -> intersectionKeys (envs ! modName) ids
            Nothing -> mempty) imports

filterNames imports envs =
    foldMap' (\(ImportDeclaration modName _ _) -> envs ! modName) imports
