module Syntax where

import Prelude hiding (lookup)
import Data.List(unsnoc)
import Data.Maybe(fromJust)
import Data.Word(Word64)
import Data.Data(Data, Typeable)
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Char(isUpper, isSymbol, isPrint)
import Data.Map.Strict(lookup, keys, foldrWithKey,
    filterWithKey, fromListWith, unionWith, keysSet)
import Data.Set qualified as Set
import Prettyprinter(Doc, Pretty, pretty, (<+>), hardline,
    dquotes, parens, brackets, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text(renderStrict)
import Data.Generics.Uniplate.Data(universe, universeBi)
import Data.Foldable(foldMap')


type Line = Word64
type Column = Word64
data Position = Position {
    line :: Line,
    column :: Column
} deriving (Show, Eq, Ord, Data, Typeable)

type StartPosition = Position
type EndPosition = Position
data Location = Location {
    startPosition :: StartPosition,
    endPosition :: EndPosition,
    filePath :: FilePath
} deriving (Show, Data, Typeable)

data Id = Id {
    text :: Text,
    location :: Location
} deriving (Show, Data, Typeable)

instance Eq Id where
    Id t1 _ == Id t2 _ = t1 == t2

instance Ord Id where
    Id t1 _ <= Id t2 _ = t1 <= t2

data Name = Name {
    qualifiers :: [Text],
    id :: Id
} deriving (Show, Data, Typeable, Eq, Ord)

data ModuleDeclaration = ModuleDeclaration {
    name :: Name,
    imports :: [ImportDeclaration],
    decls :: [Declaration]
} deriving (Show, Data, Typeable)

{-
Uses a set so that the name of a module appears only once in cases as for example:
```
import Viewer.Obj(getFaces)
import Viewer.Obj as Obj
```
-}
importedModules modDecl =
    Set.fromList [name | ImportDeclaration name _ _ <- modDecl.imports]


data Literal
    = Number Double
    | Text Text
        deriving (Show, Data, Typeable)


data Associativity = None | LeftAssociative | RightAssociative
    deriving (Show, Data, Typeable)

type Precedence = Double

type OperatorAlias = Name

type Binding = Name

type Accessor = Name

type RecordConstructor = Name

-- The imported Ids in an `import`
type Spec = Maybe [Id]

data ImportDeclaration = ImportDeclaration Name Spec (Maybe Name)
    deriving (Show, Data, Typeable)

data Declaration
    = TypeDeclaration Binding [Id] [(Binding, [Type])]
    | AliasDeclaration Binding Type
    | RecordDeclaration Binding [Id] RecordConstructor [(Accessor, Type)]
    | FixityDeclaration Associativity Precedence Binding OperatorAlias
    | TypeSignature Binding Type
    | ExpressionDeclaration Pattern Expression
    -- simplified to an expression declaration
    | FunctionDeclaration Binding [([Pattern], Expression)]
        deriving (Show, Data, Typeable)

data Type
    = TypeArrow Type Type
    | TypeOperator Type Name Type
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
    | RecordRepresentation [Id]
        deriving (Show, Data, Typeable)

data Pattern
    = VariablePattern Binding
    | LiteralPattern Literal
    | Wildcard Id
    | ConstructorPattern TagInfo Name [Pattern]
    | ParenthesizedPattern Pattern
    | ArrayPattern [Pattern]
    | OperatorPattern Pattern Name Pattern
    | AliasPattern Binding Pattern
        deriving (Show, Data, Typeable)

-- TODO think about how types could be added to expressions
-- We could define type annotation as a function with type application
-- so instead of (expression: Type) one could write (it @Type expression)
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
    | OperatorExpression Expression Name Expression
    | NegationExpression Expression
-- Await expressions are only allowed in the form
-- pat = await expr
    | AwaitExpression Expression
    | IfExpression Expression Expression Expression
    | CaseExpression Expression [(Pattern, Expression)]
    | LambdaExpression [Pattern] Expression
    | DotExpression Expression Accessor
        deriving (Show, Data, Typeable)

commas = mintercalate (text ", ")

mintercalate _ [] = mempty
mintercalate s xs = foldr1 (\x r -> x <> s <> r) xs

qualify moduleName (Name [] identifier) =
    Name (moduleName.qualifiers ++ [moduleName.id.text]) identifier
qualify _ name = name


instance Pretty Location where
    pretty location =
        pretty location.filePath
        <> text ":" <> pretty location.startPosition.line
        <> text ":" <> pretty location.startPosition.column

instance Pretty Type where
    pretty t@(TypeArrow _ _) =
        mintercalate (text " -> ") (fmap (parensType pretty) (arrowsToList t))
    pretty (TypeOperator a op b) =
        parensType pretty a <+> pretty op <+> parensType pretty b
    pretty (TypeConstructor n) = pretty n
    pretty (TypeVariable n) = pretty n
    pretty (SkolemConstant s) = text "skolem." <> pretty s
    pretty t@(TypeApplication _ _) =
        mintercalate (text " ") (fmap (parensTypeApplication pretty) (typeApplicationToList t))
    pretty (ParenthesizedType t) = parens (pretty t)
    pretty (ForAll ts t) =
        text "forall" <+> mintercalate (text " ") (fmap pretty ts)
            <> text "." <+> pretty t

parensTypeApplication f e =
    case e of
        TypeApplication _ _ -> parens (f e)
        _ -> parensType f e

parensType f e =
    case e of
        TypeOperator _ _ _ -> parens (f e)
        TypeArrow _ _ -> parens (f e)
        ForAll _ _ -> parens (f e)
        _ -> f e

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
    pretty (OperatorPattern a op b) =
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
intercalateName qualSep idSep name =
    mintercalate qualSep (fmap text name.qualifiers) <> idSep <> pretty name.id
{-# INLINE intercalateName #-}


renderError p = show (prettyError p)

renderSetToError p = show (text "Set.fromList" <+> prettyError (Set.toList p))

prettyError p = pretty p <> " at " <> pretty (mergeLocationInfos (locationInfos p))

mergeLocationInfos locations@(location:_) =
    let positions = foldMap' (\l -> [l.startPosition, l.endPosition]) locations
    in Location (minimum positions) (maximum positions) location.filePath
mergeLocationInfos _ = builtinLocation

firstLocationInfo x =
    case locationInfos x of
        loc:_ -> loc
        [] -> builtinLocation

text :: Text -> Doc a
text = pretty

prettyNumber d =
    if isInt d
        then pretty (round d :: Int)
        else pretty d

isInt d = not (isInfinite d) && d == intToDouble (round d)

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
    else Text.show c

render d = renderStrict (layoutPretty defaultLayoutOptions d)


renderEnv m = render (foldrWithKey (\k v r ->
    pretty k <+> text ":" <+> pretty v <> hardline <> r) mempty m)
{-# INLINE renderEnv #-}

nameToList name = name.qualifiers ++ [name.id.text]

fromText t =
    let (qualifiers, name) = fromJust (unsnoc (Text.split (== '.') t))
    in Name qualifiers (Id name builtinLocation)

fromId = Name []

toId (Name [] identifier) = identifier
toId name = error ("Tried to turn name " <> renderError name
    <> " into an identifier but it had qualifiers")

-- The identifier in field names is prefixed with a dot
-- so that it is easier to differentiate between variables and accessors
identifierToAccessor (Id v l) = fromId (Id (Text.cons '.' v) l)

accessorToIdentifier (Name _ (Id v l)) = Id (Text.drop 1 v) l

isConstructor name = firstIs isUpper name.id.text

isOperator name = firstIs isSym name.id.text

firstIs p t = case Text.uncons t of
    Nothing -> False
    Just (c, _) -> p c
{-# INLINE firstIs #-}

-- these are not symbols in unicode, but in the language
-- otherwise e.g 2 - 2 would not be lexed as minus
isSym x = (isSymbol x && not (isExcludedSym x)) || Text.elem x "!%&*/?@\\-:"
{-# INLINE isSym #-}

isExcludedSym x = Text.elem x "$"
{-# INLINE isExcludedSym #-}

builtinLocation = Location {
    startPosition = Position 0 0,
    endPosition = Position 0 0,
    filePath = "builtin"
}

prefixedId location x = Id ("_v" <> x) location
{-# INLINE prefixedId #-}

-- Encode the location inside the name to avoid name conflicts
locatedId location x =
    Id ("_v" <> x
        <> "_" <> uintToText location.startPosition.line
        <> "_" <> uintToText location.startPosition.column) location
{-# INLINE locatedId #-}

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

makeOpApp op a b =
    foldl' FunctionApplication (if isConstructor op
        then ConstructorExpression op
        else Variable op) [a, b]

makeOpPatCon op a b =
    ConstructorPattern TaggedRepresentation op [a, b]

makeOpTypCon op a b =
    foldl' TypeApplication (TypeConstructor op) [a, b]

-- Turns an interpolated string in the form $"{x} {y}" into a call
-- format ["", toString x, " ", toString y, ""]
makeInterpolatedString expressions =
    FunctionApplication (Variable (fromText "format")) (ArrayExpression (fmap makeInterpolatedStringPart expressions))

makeInterpolatedStringPart (LiteralExpression (Text l)) = LiteralExpression (Text l)
makeInterpolatedStringPart e = FunctionApplication (Variable (fromText "toString")) e


arrowsToList (TypeArrow x xs) = x:arrowsToList xs
arrowsToList x = [x]

typeApplicationToList (TypeApplication ts t) = typeApplicationToList ts ++ [t]
typeApplicationToList t = [t]

findEither o m = maybe (Left (notFoundMessage o m)) Right (lookup o m)

mfind o m = maybe (fail (notFoundMessage o m)) return (lookup o m)
{-# INLINE mfind #-}

firstA f (a, b) = fmap (\a' -> (a', b)) (f a)

notFoundMessage o m =
    "Unknown " <> renderError o <> " in " <> show (pretty (keys m))
{-# INLINE notFoundMessage #-}

locationInfos other =
    fmap (.location) (universeBi other :: [Id])
{-# INLINE locationInfos #-}

-- Functions for sets that error on overwriting a key
unionUnique m1 m2 = sequenceA (unionWith (bind2 shadowingError) (fmap Right m1) (fmap Right m2))

fromListUnique l = sequenceA (fromListWith (bind2 shadowingError) (fmap (fmap Right) l))

toSetUniqueM l = eitherToFail (fmap keysSet (fromListUnique (fmap dup l)))

dup x = (x, x)

bind2 f a b = do
    a' <- a
    b' <- b
    f a' b'

shadowingError v1 v2 =
    Left (renderError v1 <> " shadows " <> renderError v2)

eitherToFail e = either fail return e

curryLambda ps e =
    foldr (\p -> LambdaExpression [p]) e ps

identical (x:xs) =
    if all (== x) xs
        then x
        else error ("Not all elements are equivalent to "
            ++ show x ++ " in " ++ show xs)
identical [] = error "Called `identical` with empty list"

intToDouble :: Int -> Double
intToDouble = fromIntegral

intToText :: Int -> Text
intToText = Text.show

uintToText :: Word64 -> Text
uintToText = Text.show
