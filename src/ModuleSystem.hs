module ModuleSystem where

import Syntax
import Aliases
import Simplifier
import Sorting
import Parser
import TypeChecker
import Operators
import Qualification
import Data.List(nub)
import Control.Arrow(first)
import System.FilePath(takeDirectory, takeBaseName, (</>))
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

pipeline :: [(ModuleDeclaration, [(Name, Maybe [Id])])] -> [(ModuleDeclaration, [(Name, Maybe [Id])])]
pipeline = sortDeclsMod . aliasProgram
    . aliasOperatorsProgram . removeParens
    . fixAssocProgram . qualification
    . simplifications . sortModules

qualification = fmap (first qualifyM)

-- TODO search path
-- Allow qualified modules
-- Search A.B in A.B.hyp and A/B.hyp
toPath dir name = dir </> name ++ ".hyp"

loadModule dir modName =
    let path = toPath dir (pretty modName)
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
typechecking = feedbackM logEnv typecheckModule []

logEnv env mod =
    do
        let modName = getName mod
        writeFile ("logs/" ++ pretty modName ++ ".log") (prettyEnv env)
        return mod

{- Operators and Aliasing -}
fixAssocProgram = performSimple fixAssoc captureAssocs

aliasOperatorsProgram = performSimple aliasOperators captureOperatorAliases

aliasProgram = performSimple aliasTypes captureAliases

performSimple action capture = feedback action (captureSimple capture) []

captureSimple capture importedTable mod =
    capture mod `mappend` importedTable

{-
Incrementally grow an environment and perform an action on all modules
capture : captures the environment e.g. the operators and their aliases
envs : an association list of the module name and its captured environment
-}
feedbackM action capture envs mods =
    fmap fst (mapAccumM (step action capture) envs mods)

feedback action capture envs mods =
    feedbackM (\a b -> return (action a b)) (\a b -> return (capture a b)) envs mods ()

step action capture envs (mod, imports) =
    do
        captured <- capture (filterEnvs imports envs) mod
        result <- action captured mod
        return ((result, imports), (getName mod, captured):envs)

mapAccumM f s t = runStateT (traverse (StateT . flip f) t) s

filterEnvs imports envs =
    foldMap snd (filter (\(k, _) -> elem k (fmap fst imports)) envs)

gatherImports decls = 
    [(name, spec) | ImportDeclaration name spec _ <- decls]
