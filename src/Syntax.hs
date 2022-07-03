{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Syntax where

import Prelude hiding (lookup)
import Data.Word(Word64)
import Data.Data(Data, Typeable)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Char(isUpper, isSymbol)
import Data.Hashable(Hashable, hashWithSalt)
import Data.HashMap.Strict(lookup, keys, foldrWithKey,
    filterWithKey, fromListWith, unionWith)
import Prettyprinter(Doc, Pretty, pretty, (<+>), hardline,
    parens, brackets, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text(renderStrict)
import Data.Generics.Uniplate.Data(universe, universeBi)
import Data.Foldable(foldMap')


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


instance Pretty Location where
    pretty (Location (Position line column) _ filePath) =
        pretty filePath
        <> text ":" <> pretty line
        <> text ":" <> pretty column

instance Pretty Type where
    pretty (TypeArrow a b) =
        parens (pretty a <+> text "->" <+> pretty b)
    pretty (TypeInfixOperator a op b) =
        parens (pretty a <+> pretty op <+> pretty b)
    pretty (TypeConstructor n) = pretty n
    pretty (TypeVariable n) = pretty n
    pretty (SkolemConstant s) = text "skolem." <> pretty s
    pretty (TypeApplication a b) = parens (pretty a <+> pretty b)
    pretty (ParenthesizedType t) = parens (pretty t)
    pretty (ForAll ts t) =
        text "forall" <+> mintercalate (text " ") (fmap pretty ts)
            <> text "." <+> pretty t

instance Pretty Literal where
    pretty (Numeral n) = pretty n
    pretty (Text t) = prettyEscaped t

instance Pretty Pattern where
    pretty (VariablePattern v) = pretty v
    pretty (LiteralPattern l) = pretty l
    pretty (Wildcard _) = text "_"
    pretty (ConstructorPattern name []) =
        pretty name
    pretty (ConstructorPattern name ps) =
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
    pretty = flatName (text ".") (text ".")

instance Pretty Id where
    pretty (Id i _) = text i

{- Rendering Names -}
renderName modName = show (pretty modName)

toPath name =
    show (flatName (text "/") (text "/") name <> text ".hyp")

-- `flat` means displaying the module name joined with underscores instead of dots
flatName _ _ (Name [] i) = pretty i
flatName qualSep idSep (Name qs i) =
    mintercalate qualSep (fmap text qs) <> idSep <> pretty i
{-# INLINE flatName #-}

flatVar = flatName (text "_") (text ".")
{-# INLINE flatVar #-}

-- TODO what would be a good escape character in file names?
-- Dots cannot be used in Lua, because they have a special meaning:
-- `local module = require "folder.file"`
-- For now underscores are used. However, as a consequence underscore should be disallowed.
flatModName = flatName (text "_") (text "_")
{-# INLINE flatModName #-}

renderError p = show (prettyError p)

prettyError p = pretty p <> " at " <> pretty (mergeLocationInfos (locationInfos p))

mergeLocationInfos locations@(Location _ _ filePath:_) =
    let positions = mconcat [[startPosition, endPosition] | Location startPosition endPosition _ <- locations] in
        Location (minimum positions) (maximum positions) filePath
mergeLocationInfos _ = builtinLocation

text :: Text -> Doc a
text = pretty

prettyNumeral d =
    if not (isInfinite d) && d == intToDouble (round d)
        then pretty (round d :: Int)
        else pretty d

-- `show` escapes and creates double quotes
prettyEscaped = text . Text.pack  . show

render d = renderStrict (layoutPretty defaultLayoutOptions d)


renderEnv m = render (foldrWithKey (\k v r ->
    pretty k <+> text ":" <+> pretty v <> hardline <> r) mempty m)
{-# INLINE renderEnv #-}

nameToList (Name is i) = is ++ [getText i]

fromText s =
    let is = Text.split (== '.') s
    in Name (init is) (Id (last is) builtinLocation)

fromId = Name []

toId (Name [] identifier) = identifier
toId n = error ("Name " <> renderError n <> " has qualifiers")

isConstructor (Name _ i) = firstIs isUpper (getText i)

isOperator (Name _ i) = firstIs isSym (getText i)

firstIs f = Text.foldr (const . f) False
{-# INLINE firstIs #-}

-- these are not symbols in unicode, but in the language
-- otherwise e.g 2 - 2 would not be lexed as minus
isSym x = isSymbol x || Text.elem x "!%&*/?@-:"
{-# INLINE isSym #-}

isUnqualified (Name [] _) = True
isUnqualified _ = False

builtinLocation = Location (Position 0 0) (Position 0 0) "builtin"

prefixedId x = Id ("_v" <> x) builtinLocation
{-# INLINE prefixedId #-}

getQualifiers (Name q _) = q

getText (Id t _) = t

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

nNewVars n = fmap (prefixedId . intToText) [1..n]

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

notFoundMessage o m =
    "Unknown " <> renderError o <> " in " <> renderError (keys m)
{-# INLINE notFoundMessage #-}

locationInfos other = [l | Id _ l <- universeBi other]
{-# INLINE locationInfos #-}

-- Functions for maps that error on overwriting a key
fromListUnique xs = fromListWith shadowingError xs

shadowingError v1 v2 =
    error (renderError v1 <> " shadows " <> renderError v2)

unionUnique m = unionWith shadowingError m

intToDouble :: Int -> Double
intToDouble = fromIntegral

intToText :: Int -> Text
intToText = Text.pack . show

uintToText :: Word64 -> Text
uintToText = Text.pack . show
