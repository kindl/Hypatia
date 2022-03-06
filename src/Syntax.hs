{-# LANGUAGE DeriveDataTypeable #-}
module Syntax where

import Prelude hiding (lookup, (<>))
import Data.Word(Word64)
import Data.Data(Data, Typeable)
import Data.Text(Text, unpack, pack, split)
import qualified Data.Text as Text
import Data.Char(isUpper, isSymbol)
import Data.Hashable(Hashable, hashWithSalt)
import Data.HashMap.Strict(lookup, keys, foldrWithKey,
    filterWithKey, fromListWith, unionWith)
import Text.PrettyPrint.HughesPJClass(Pretty, pPrint)
import Text.PrettyPrint(text, (<+>), ($$), (<>),
    parens, brackets, render)
import Data.Generics.Uniplate.Data(universe, universeBi)
import Data.Foldable(foldMap')


type Line = Word64
type Column = Word64
data Position = Position Line Column
    deriving (Show, Data, Typeable)

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
    ModuleDeclaration Name [Declaration]
        deriving (Show, Data, Typeable)

getDecls (ModuleDeclaration _ decls) = decls
getName (ModuleDeclaration name _) = name

gatherImports modDecl =
    fmap fst (gatherSpecs modDecl)

gatherSpecs modDecl = 
    [(name, spec) | ImportDeclaration name spec _ <- getDecls modDecl]


data Literal
    = Numeral Double
    | Text Text
        deriving (Show, Data, Typeable)


data Associativity = None | LeftAssociative | RightAssociative
    deriving (Show, Data, Typeable)

type Precedence = Double

type OperatorAlias = Name

type Binding = Name

data Declaration
    = ImportDeclaration Name (Maybe [Id]) (Maybe Name)
    | TypeDeclaration Binding [Id] [(Binding, [Type])]
    | AliasDeclaration Binding Type
    | FixityDeclaration Associativity Precedence Binding OperatorAlias
    | TypeSignature Binding Type
    | ExpressionDeclaration Pattern Expression
    -- simplified to an expression declaration
    | FunctionDeclaration Binding [Pattern] Expression
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


data Pattern
    = VariablePattern Binding
    | LiteralPattern Literal
    | Wildcard Id
    | ConstructorPattern Name [Pattern]
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

pretty p = render (pPrint p)
{-# INLINE pretty #-}

instance Pretty Position where
    pPrint (Position l c) =
        text "line" <+> text (show l) <> text ", column" <+> text (show c)

instance Pretty Location where
    pPrint (Location (Position line column) _ filePath) =
        text filePath
        <> text ":" <> text (show line)
        <> text ":" <> text (show column)

instance Pretty Type where
    pPrint (TypeArrow a b) =
        parens (pPrint a <+> text "->" <+> pPrint b)
    pPrint (TypeInfixOperator a op b) =
        parens (pPrint a <+> prettyName op <+> pPrint b)
    pPrint (TypeConstructor n) = prettyName n
    pPrint (TypeVariable n) = prettyId n
    pPrint (SkolemConstant s) = text "skolem." <> pPrint s
    pPrint (TypeApplication a b) = parens (pPrint a <+> pPrint b)
    pPrint (ParenthesizedType t) = parens (pPrint t)
    pPrint (ForAll ts t) =
        text "forall" <+> mintercalate (text " ") (fmap prettyId ts)
            <> text "." <+> pPrint t

instance Pretty Literal where
    pPrint (Numeral n) = pPrint n
    pPrint (Text t) = text (show t)

instance Pretty Pattern where
    pPrint (VariablePattern v) = prettyName v
    pPrint (LiteralPattern l) = pPrint l
    pPrint (Wildcard _) = text "_"
    pPrint (ConstructorPattern name []) =
        pPrint name
    pPrint (ConstructorPattern name ps) =
        parens (prettyName name
            <+> mintercalate (text " ") (fmap pPrint ps))
    pPrint (PatternInfixOperator a op b) =
        parens (pPrint a <+> prettyName op <+> pPrint b)
    pPrint (ParenthesizedPattern p) =
        parens (pPrint p)
    pPrint (AliasPattern v p) =
        prettyName v <+> text "as" <+> pPrint p
    pPrint (ArrayPattern ps) =
        brackets (commas (fmap pPrint ps))

instance Pretty Name where
    pPrint modName@(Name _ identifier) =
        prettyName modName <+> pPrint (getLocation identifier)

instance Pretty Id where
    pPrint (Id s l) = text (unpack s) <+> text "at" <+> pPrint l

-- displays the module name joined with underscore
flatVar = flatName (text "_") (text ".")
{-# INLINE flatVar #-}

-- TODO what would be a good character for file names?
-- In Lua dots have a special meaning, so we use underscore instead
--local module = require "folder.file"
-- However, we would need to disallow module names with underscore in it
flatModName = flatName (text "_") (text "_")

prettyName = flatName (text ".") (text ".")

renderName modName = render (prettyName modName)

renderFlatModName modName = render (flatModName modName)

toPath name =
    render (flatName (text "/") (text "/") name <> text ".hyp")

flatName _ _ (Name [] i) = prettyId i
flatName qualSep idSep (Name qs i) =
    mintercalate qualSep (fmap (text . unpack) qs)
        <> idSep <> prettyId i
{-# INLINE flatName #-}

prettyId i = text (unpack (getText i))
{-# INLINE prettyId #-}

renderEnv m = render (foldrWithKey (\k v r ->
    prettyName k <+> text ":" <+> pPrint v $$ r) mempty m)
{-# INLINE renderEnv #-}

nameToList (Name is i) = is ++ [getText i]

fromText s =
    let is = split (== '.') s
    in Name (init is) (Id (last is) builtinLocation)

fromId = Name []

toId (Name [] identifier) = identifier
toId n = error ("Name " ++ pretty n ++ " has qualifiers")

isConstructor (Name _ i) = firstIs isUpper (getText i)

isOperator (Name _ i) = firstIs isSym (getText i)

firstIs f = Text.foldr (const . f) False
{-# INLINE firstIs #-}

-- these are not symbols in unicode, but in the language
-- otherwise e.g 2 - 2 would not be lexed as minus
isSym x = isSymbol x || elem x "!%&*/?@-:"
{-# INLINE isSym #-}

isUnqualified (Name [] _) = True
isUnqualified _ = False

builtinLocation = Location (Position 0 0) (Position 0 0) "builtin"

prefixedId x = Id (pack ("_v" ++ x)) builtinLocation
{-# INLINE prefixedId #-}

getQualifiers (Name q _) = q

getText (Id t _) = t

getLocation (Id _ l) = l

-- A flipped version of intersect that works on foldables
including xs = filter (flip elem xs)
{-# INLINE including #-}

-- Similar to a flipped version of difference
-- but deletes all, not only single occurences
excluding xs = filter (flip notElem xs)
{-# INLINE excluding #-}

excludingKeys xs = filterWithKey (const . flip notElem xs)
{-# INLINE excludingKeys #-}

includingKeys xs = filterWithKey (const . flip elem xs)
{-# INLINE includingKeys #-}

getDefsD (ExpressionDeclaration p _) = getDefsP p
getDefsD (TypeDeclaration _ _ cs) = fmap fst cs
getDefsD (TypeSignature s _) = [s]
getDefsD _ = []

getDefsP p =
    let
        f (VariablePattern v) = [v]
        f (AliasPattern i _) = [i]
        f _ = []
    in foldMap' f (universe p)

nNewVars n = fmap (prefixedId . show) [1..n]

makeOp op a b =
    FunctionApplication (FunctionApplication
        (if isConstructor op then
            ConstructorExpression op else Variable op) a) b

makeOpPat op a b =
    ConstructorPattern op [a, b]

makeOpTyp op a b =
    TypeApplication (TypeApplication (TypeConstructor op) a) b

findEither o m = maybe (Left (notFoundMessage o m)) Right (lookup o m)

mfind o m = maybe (fail (notFoundMessage o m)) return (lookup o m)
{-# INLINE mfind #-}

firstA f (a, b) = fmap (\a' -> (a', b)) (f a)

notFoundMessage o m = "Unknown " ++ pretty o ++ " in "
    ++ pretty (keys m)
{-# INLINE notFoundMessage #-}

locationInfo other = pretty [l | Id _ l <- universeBi other]
{-# INLINE locationInfo #-}

-- Functions for maps that error on overwriting a key
fromListUnique xs = fromListWith shadowingError xs

shadowingError v1 v2 = error (pretty v1
    ++ " shadows " ++ pretty v2)

unionUnique m = unionWith shadowingError m

intToDouble :: Int -> Double
intToDouble = fromIntegral
