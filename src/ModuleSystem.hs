module ModuleSystem where

import Syntax
import Aliases
import Simplifier
import Sorting
import Parser
import TypeChecker
import Operators
import Qualification
import Data.List(nub, partition)
import Control.Arrow(first)
import System.FilePath(takeDirectory, takeBaseName, (</>))

loadProgram path =
  do
    let dir = takeDirectory path
    let modName = fromString (takeBaseName path)

    mod <- loadModuleWithSpec dir modName
    mods <- growModuleEnv dir [mod]
    let sorted = sortModules [] mods
    let simplified = pipeline sorted
    typechecking simplified

    return (fmap fst simplified)

pipeline :: [(ModuleDeclaration, [(Name, Maybe [Id])])] -> [(ModuleDeclaration, [(Name, Maybe [Id])])]
pipeline = sortDeclsMod . aliasProgram
    . aliasOperatorsProgram . removeParens
    . fixAssocProgram . qualification
    . simplifications

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
    let localTable = capture mod
    in localTable `mappend` importedTable

{-
Grow an environment and perform an action on all modules
capture : captures the environment e.g. the operators and their aliases
envs : an assoc list of the mod name and the finished action
-}
feedbackM _ _ _ [] = return []
feedbackM action capture envs ((mod, imports):rest) =
    let importedEnv = filterEnvs imports envs
    in do
        captured <- capture importedEnv mod
        result <- action captured mod
        results <- feedbackM action capture ((getName mod, captured):envs) rest
        return ((result, imports):results)

feedback action capture envs specMods =
    feedbackM (\a b -> return (action a b)) (\a b -> return (capture a b)) envs specMods ()

filterEnvs imports envs =
    foldMap snd (filter (\(k, _) -> elem k (fmap fst imports)) envs)

--TODO allow cyclic module imports?
sortModules sorted [] = sorted
sortModules sorted specMods =
    let checked = map (getName . fst) sorted
    in case partition (isCandidate checked) specMods of
         ([], _) -> error "Cyclic module dependency"
         (candidates, rest) -> sortModules (sorted ++ candidates) rest

isCandidate checked (_, imports) =
    null (excluding checked (map fst imports))

getDecls (ModuleDeclaration _ decls) = decls
getName (ModuleDeclaration name _) = name

gatherImports decls = 
    [(name, spec) | ImportDeclaration name spec <- decls]
