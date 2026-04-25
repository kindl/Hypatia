module Transformations where

import Syntax
import Aliases
import Simplifier
import Sorting
import Typechecker
import Operators
import Qualification
import Data.Map.Strict(insert, (!))
import Data.Foldable(foldMap')
import Control.Monad((<=<))
-- import qualified Data.Text.IO as Text
import Data.Traversable(mapAccumM)


transformProgram ms = eitherToFail (transformations ms)

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

typecheckAction envs modDecl = do
    let filtered = filterNames modDecl.getImports envs
    captured <- typecheckModule filtered modDecl
    -- Save the environment for debugging
    -- let logPath = "logs/" ++ renderName modDecl.getName ++ ".log"
    -- Text.writeFile logPath (renderEnv captured)
    return (captured, ())

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
    feedbackM (simpleActionA annotateTagInfo filterNames (return . captureTagInfos))

-- Run action with captured local env and imported envs
-- but return only the local environment
simpleActionA action filterEnvs capture envs modDecl = do
    captured <- capture modDecl
    let combined = captured <> filterEnvs modDecl.getImports envs
    result <- action combined modDecl
    pure (captured, result)

-- Incrementally grow an environment and perform an action on all modules.
-- An action is a function that returns captured information e.g. operators and their aliases
-- and a changed module e.g. where operators have been changed to function application.
feedbackM action mods = fmap snd (mapAccumM step mempty mods)
    where step envs modDecl = do
            (captured, result) <- action envs modDecl
            return (insert modDecl.getName captured envs, result)

filterIds imports envs =
    foldMap' (\(ImportDeclaration modName importedIds _) ->
        case importedIds of
            Just ids -> intersectionKeys (envs ! modName) ids
            Nothing -> mempty) imports

filterNames imports envs =
    foldMap' (\(ImportDeclaration modName _ _) -> envs ! modName) imports
