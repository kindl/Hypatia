{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Syntax where

import Prelude hiding (lookup)
import Data.Word(Word64)
import Data.Data(Data, Typeable)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Char(isUpper, isSymbol, isPrint)
import Data.Hashable(Hashable, hashWithSalt)
import Data.HashMap.Strict(lookup, keys, foldrWithKey,
    filterWithKey, fromListWith, unionWith, keysSet)
import qualified Data.HashSet as Set
import Prettyprinter(Doc, Pretty, pretty, (<+>), hardline,
    dquotes, parens, brackets, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text(renderStrict)
import Data.Generics.Uniplate.Data(universe, universeBi)
import Data.Foldable(foldMap', foldl')


type Line = Word64
type Column = Word64
data Position = Position Line Column
    deriving (Show, Eq, Ord, Data, Typeable)

type StartPosition = Position
type EndPosition = Position
data Location = Location StartPosition EndPosition FilePath
    deriving (Show, Data, Typeable)

data Id = Id Text Location
    deriving (Show, Data, Typeable)

instance Eq Id where
    Id t1 _ == Id t2 _ = t1 == t2

instance Hashable Id where
    hashWithSalt s (Id t _) = hashWithSalt s t

data Name = Name [Text] Id
    deriving (Show, Data, Typeable, Eq)

instance Hashable Name where
    hashWithSalt s (Name a b) = hashWithSalt s (a, b)


data ModuleDeclaration =
    ModuleDeclaration Name [ImportDeclaration] [Declaration]
        deriving (Show, Data, Typeable)

getDecls (ModuleDeclaration _ _ decls) = decls
getName (ModuleDeclaration name _ _) = name
getImports (ModuleDeclaration _ imports _) = imports

{-
Uses a set so that the name of a module appears only once in cases as for example:
```
import Viewer.Obj(getFaces)
import Viewer.Obj as Obj
```
-}
importedModules (ModuleDeclaration _ imports _) =
    Set.fromList [name | ImportDeclaration name _ _ <- imports]


data Literal
    = Number Double
    | Text Text
        deriving (Show, Data, Typeable)


data Associativity = None | LeftAssociative | RightAssociative
    deriving (Show, Data, Typeable)

type Precedence = Double

type OperatorAlias = Name

type Binding = Name

-- The imported Ids in an `import`
type Spec = Maybe [Id]

data ImportDeclaration = ImportDeclaration Name Spec (Maybe Name)
    deriving (Show, Data, Typeable)

data Declaration
    = TypeDeclaration Binding [Id] [(Binding, [Type])]
    | AliasDeclaration Binding Type
    | FixityDeclaration Associativity Precedence Binding OperatorAlias
    | TypeSignature Binding Type
    | ExpressionDeclaration Pattern Expression
    -- simplified to an expression declaration
    | FunctionDeclaration Binding [([Pattern], Expression)]
        deriving (Show, Data, Typeable)


data Type
    = TypeArrow Type Type
    | TypeInfixOperator Type Name Type
    | TypeApplication Type Type
    | TypeConstructor Name
    | ParenthesizedType Type
    | ForAll [Id] Type
    | TypeVariable Id
    | SkolemConstant Id
        deriving (Show, Data, Typeable)

data TagInfo =
    TaggedRepresentation
    | ArrayRepresentation
        deriving (Show, Data, Typeable)

data Pattern
    = VariablePattern Binding
    | LiteralPattern Literal
    | Wildcard Id
    | ConstructorPattern TagInfo Name [Pattern]
    | ParenthesizedPattern Pattern
    | ArrayPattern [Pattern]
    | PatternInfixOperator Pattern Name Pattern
    | AliasPattern Binding Pattern
        deriving (Show, Data, Typeable)


data Expression
    = Variable Name
    | LiteralExpression Literal
    | ConstructorExpression Name
    | FunctionApplication Expression Expression
    | LetExpression [Declaration] Expression
-- TODO think about a nicer way to add case lambdas
-- It should have a way to match on more than just a single value
-- Then function declarations could be easily turned into case lambdas as well
--  | CaseLambdaExpression [(Pattern, Expression)]
    | ParenthesizedExpression Expression
    | ArrayExpression [Expression]
    | InfixOperator Expression Name Expression
    | PrefixNegation Expression
-- Await expressions are only allowed in the form
-- pat = await expr
    | AwaitExpression Expression
    | IfExpression Expression Expression Expression
    | CaseExpression Expression [(Pattern, Expression)]
    | LambdaExpression [Pattern] Expression
-- TODO think about how types could be added to expressions
-- We could define type annotation as a function with type application
--  | TypeAnnotation Expression Type
        deriving (Show, Data, Typeable)

commas = mintercalate (text ", ")

mintercalate _ [] = mempty
mintercalate s xs = foldr1 (\x r -> x `mappend` s `mappend` r) xs

qualify (Name modQs modName) (Name [] name) =
    Name (modQs ++ [getText modName]) name
qualify _ n = n


instance Pretty Location where
    pretty (Location (Position line column) _ filePath) =
        pretty filePath
        <> text ":" <> pretty line
        <> text ":" <> pretty column

instance Pretty Type where
    pretty t@(TypeArrow _ _) =
        parens (mintercalate (text " -> ") (fmap pretty (arrowsToList t)))
    pretty (TypeInfixOperator a op b) =
        parens (pretty a <+> pretty op <+> pretty b)
    pretty (TypeConstructor n) = pretty n
    pretty (TypeVariable n) = pretty n
    pretty (SkolemConstant s) = text "skolem." <> pretty s
    pretty t@(TypeApplication _ _) =
        parens (mintercalate (text " ") (fmap pretty (typeApplicationToList t)))
    pretty (ParenthesizedType t) = parens (pretty t)
    pretty (ForAll ts t) =
        text "forall" <+> mintercalate (text " ") (fmap pretty ts)
            <> text "." <+> pretty t

instance Pretty Literal where
    pretty (Number n) = pretty n
    pretty (Text t) = prettyEscaped t

instance Pretty Pattern where
    pretty (VariablePattern v) = pretty v
    pretty (LiteralPattern l) = pretty l
    pretty (Wildcard _) = text "_"
    pretty (ConstructorPattern _ name []) =
        pretty name
    pretty (ConstructorPattern _ name ps) =
        parens (pretty name
            <+> mintercalate (text " ") (fmap pretty ps))
    pretty (PatternInfixOperator a op b) =
        parens (pretty a <+> pretty op <+> pretty b)
    pretty (ParenthesizedPattern p) =
        parens (pretty p)
    pretty (AliasPattern v p) =
        pretty v <+> text "as" <+> pretty p
    pretty (ArrayPattern ps) =
        brackets (commas (fmap pretty ps))

instance Pretty Name where
    pretty = intercalateName (text ".") (text ".")

instance Pretty Id where
    pretty (Id i _) = text i

{- Rendering Names -}
renderName modName = show (pretty modName)

toPath name =
    show (intercalateName (text "/") (text "/") name <> text ".hyp")

-- Join the module name with underscores instead of dots
-- The underscore is used as escape character in file names,
-- because dots cannot be used in Lua due to their special meaning:
-- `local module = require "folder.file"`
-- TODO Disallow/escape underscores in module names or find a better escape character
flatModName = intercalateName (text "_") (text "_")
{-# INLINE flatModName #-}

flatName = intercalateName (text "_") (text ".")
{-# INLINE flatName #-}

intercalateName _ _ (Name [] i) = pretty i
intercalateName qualSep idSep (Name qs i) =
    mintercalate qualSep (fmap text qs) <> idSep <> pretty i
{-# INLINE intercalateName #-}


renderError p = show (prettyError p)

renderSetToError p = show (text "Set.fromList" <+> prettyError (Set.toList p))

prettyError p = pretty p <> " at " <> pretty (mergeLocationInfos (locationInfos p))

mergeLocationInfos locations@(Location _ _ filePath:_) =
    let positions = mconcat [[startPosition, endPosition] | Location startPosition endPosition _ <- locations] in
        Location (minimum positions) (maximum positions) filePath
mergeLocationInfos _ = builtinLocation

text :: Text -> Doc a
text = pretty

prettyNumber d =
    if not (isInfinite d) && d == intToDouble (round d)
        then pretty (round d :: Int)
        else pretty d

prettyEscaped = dquotes . text . Text.concatMap escape

escape '\a' = "\\a"
escape '\f' = "\\f"
escape '\n' = "\\n"
escape '\r' = "\\r"
escape '\t' = "\\t"
escape '\v' = "\\v"
escape '\"' = "\\\""
escape '\'' = "\\\'"
escape '\\' = "\\\\"
escape c = if isPrint c
    then Text.singleton c
    else Text.pack (show c)

render d = renderStrict (layoutPretty defaultLayoutOptions d)


renderEnv m = render (foldrWithKey (\k v r ->
    pretty k <+> text ":" <+> pretty v <> hardline <> r) mempty m)
{-# INLINE renderEnv #-}

nameToList (Name is i) = is ++ [getText i]

fromText t =
    let splitted = Text.split (== '.') t
    in Name (init splitted) (Id (last splitted) builtinLocation)

inputToModuleName s =
    if Text.isSuffixOf ".hyp" s || Text.elem '/' s || Text.elem '\\' s
        then fail "Expected a module name instead of a path"
        else return (fromText s)

fromId = Name []

toId (Name [] identifier) = identifier
toId n = error ("Tried to turn name " <> renderError n
    <> " into an identifier but it had qualifiers")

isConstructor (Name _ i) = firstIs isUpper (getText i)

isOperator (Name _ i) = firstIs isSym (getText i)

firstIs f = Text.foldr (const . f) False
{-# INLINE firstIs #-}

-- these are not symbols in unicode, but in the language
-- otherwise e.g 2 - 2 would not be lexed as minus
isSym x = (isSymbol x && not (isExcludedSym x)) || Text.elem x "!%&*/?@\\-:"
{-# INLINE isSym #-}

isExcludedSym x = Text.elem x "$"
{-# INLINE isExcludedSym #-}

builtinLocation = Location (Position 0 0) (Position 0 0) "builtin"

prefixedId location x = Id ("_v" <> x) location
{-# INLINE prefixedId #-}

getQualifiers (Name q _) = q

getText (Id t _) = t

getLocation (Id _ l) = l

getId (Name _ i) = i

differenceKeys m ks = filterWithKey (const . flip notElem ks) m
{-# INLINE differenceKeys #-}

intersectionKeys m ks = filterWithKey (const . flip elem ks) m
{-# INLINE intersectionKeys #-}

getBindings p =
    let
        f (VariablePattern v) = [v]
        f (AliasPattern i _) = [i]
        f _ = []
    in foldMap' f (universe p)

nNewVars n =
    fmap (prefixedId builtinLocation . intToText) [1..n]

makeOp op a b =
    foldl' FunctionApplication (if isConstructor op
        then ConstructorExpression op
        else Variable op) [a, b]

makeOpPat op a b =
    ConstructorPattern TaggedRepresentation op [a, b]

makeOpTyp op a b =
    foldl' TypeApplication (TypeConstructor op) [a, b]

makeInterpolatedString start expressions end =
    let vars = ArrayExpression (start : expressions ++ [end])
    in FunctionApplication (Variable (fromText "format")) vars

arrowsToList (TypeArrow x xs) = x:arrowsToList xs
arrowsToList x = [x]

typeApplicationToList (TypeApplication ts t) = typeApplicationToList ts ++ [t]
typeApplicationToList t = [t]

findEither o m = maybe (Left (notFoundMessage o m)) Right (lookup o m)

mfind o m = maybe (fail (notFoundMessage o m)) return (lookup o m)
{-# INLINE mfind #-}

firstA f (a, b) = fmap (\a' -> (a', b)) (f a)

notFoundMessage o m =
    "Unknown " <> renderError o <> " in " <> renderError (keys m)
{-# INLINE notFoundMessage #-}

locationInfos other = [l | Id _ l <- universeBi other]
{-# INLINE locationInfos #-}

-- Functions for sets that error on overwriting a key
unionUnique m1 m2 = sequenceA (unionWith (bind2 shadowingError) (fmap Right m1) (fmap Right m2))

fromListUnique l = sequenceA (fromListWith (bind2 shadowingError) (fmap ((,) <*> Right) l))

toSetUniqueM l = either fail return (fmap keysSet (fromListUnique l))

bind2 f a b = do
    a' <- a
    b' <- b
    f a' b'

shadowingError v1 v2 =
    Left (renderError v1 <> " shadows " <> renderError v2)

curryLambda ps e =
    foldr (\p -> LambdaExpression [p]) e ps

intToDouble :: Int -> Double
intToDouble = fromIntegral

intToText :: Int -> Text
intToText = Text.pack . show

uintToText :: Word64 -> Text
uintToText = Text.pack . show
