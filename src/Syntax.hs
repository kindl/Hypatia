{-# LANGUAGE DeriveDataTypeable #-}
module Syntax where

import Prelude hiding (lookup)
import Data.Data
import Data.Text(Text, unpack, pack, split)
import Data.Char(isUpper, isSymbol)
import Control.Monad(MonadPlus, mzero, mplus)
import Control.Applicative(Alternative, empty, (<|>))
import Data.Hashable(Hashable, hashWithSalt)
import Data.HashMap.Strict(lookup, keys, foldrWithKey)
import Text.PrettyPrint.HughesPJClass(Pretty, pPrint)
import Text.PrettyPrint(text, (<+>), ($$), parens, brackets, render)
import Data.Generics.Uniplate.Data(universe)

type Line = Integer
type Column = Integer
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

data Name = Name [Id] Id
    deriving (Show, Data, Typeable, Eq)

instance Hashable Name where
    hashWithSalt s (Name a b) = hashWithSalt s (a, b)

type Precedence = Integer

data ModuleDeclaration =
    ModuleDeclaration Name [Declaration]
        deriving (Show, Data, Typeable)


data Literal
    = Numeral Double
    | Text Text
        deriving (Show, Data, Typeable)


data Associativity = None | LeftAssociative | RightAssociative
    deriving (Show, Data, Typeable)

stringToFixity "infixl" = LeftAssociative
stringToFixity "infixr" = RightAssociative
stringToFixity "infix" = None
stringToFixity x = error ("Unknow associativity " ++ x)


data Declaration
    = ImportDeclaration Name (Maybe [Id])
    | EnumDeclaration Id [Id] [(Id, [Type])]
    | ExpressionDeclaration Pattern Expression
    | AliasDeclaration Id Type
    -- simplified to an expression declaration
    | FunctionDeclaration Id [Pattern] Expression
    | FixityDeclaration Associativity Precedence Id Id
    | TypeSignature Id Type
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
        deriving (Data, Typeable)

instance Show Type where
    show = pretty

data Pattern
    = VariablePattern Id
    | LiteralPattern Literal
    | Wildcard
    | ConstructorPattern Name [Pattern]
    | ParenthesizedPattern Pattern
    | ArrayPattern [Pattern]
    | PatternInfixOperator Pattern Name Pattern
    | AliasPattern Id Pattern
        deriving (Show, Data, Typeable)


data Expression
    = Variable Name
    | LiteralExpression Literal
    | ConstructorExpression Name
    | FunctionApplication Expression Expression
    | LetExpression [Declaration] Expression
    | CaseLambdaExpression [(Pattern, Expression)]
    | ParenthesizedExpression Expression
    | ArrayExpression [Expression]
    | InfixOperator Expression Name Expression
    | PrefixNegation Expression
    | IfExpression Expression Expression Expression
    | CaseExpression Expression [(Pattern, Expression)]
    | LambdaExpression [Pattern] Expression
-- TODO think about how types could be added to expressions
-- We could define type annotation as a function with type application
--  | TypeAnnotation Expression Type
        deriving (Show, Data, Typeable)

mintercalate _ [] = mempty
mintercalate p (x:xs) =
    x `mappend` foldMap (mappend p) xs

qualify (Name modQs modName) (Name [] name) = Name (modQs ++ [modName]) name
qualify _ n = n

qualifyId q n = qualify q (fromId n)

pretty p = render (pPrint p)

instance Pretty Position where
    pPrint (Position l c) =
        text "line:" <+> pPrint l <+> text "column:" <+> pPrint c

instance Pretty Location where
    pPrint (Location s e f) =
        text "start" <+> parens (pPrint s)
            <+> text "end" <+> parens (pPrint e)
            <+> text "in" <+> pPrint f    

instance Pretty Type where
    pPrint (TypeArrow a b) = parens (pPrint a <+> text "->" <+> pPrint b)
    pPrint (TypeInfixOperator a op b) =
        parens (pPrint a <+> pPrint op <+> pPrint b)
    pPrint (TypeConstructor n) = pPrint n
    pPrint (TypeVariable n) = pPrint n
    pPrint (SkolemConstant s) = text "skolem." <+> pPrint s
    pPrint (TypeApplication a b) = parens (pPrint a <+> pPrint b)
    pPrint (ParenthesizedType t) = parens (pPrint t)
    pPrint (ForAll qual t) =
        text "forall" <+> mintercalate (text " ") (fmap pPrint qual) <+> text "." <+> pPrint t

instance Pretty Literal where
    pPrint (Numeral n) = pPrint n
    pPrint (Text t) = text (show t)

instance Pretty Pattern where
    pPrint (VariablePattern id) = pPrint id
    pPrint (LiteralPattern l) = pPrint l
    pPrint Wildcard = text "_"
    pPrint (ConstructorPattern name ps) =
        parens (pPrint name <+> mintercalate (text " ") (fmap pPrint ps))
    pPrint (PatternInfixOperator a op b) =
        parens (pPrint a <+> pPrint op <+> pPrint b)
    pPrint (ParenthesizedPattern p) =
        parens (pPrint p)
    pPrint (AliasPattern i p) =
        pPrint i <+> text "as" <+> pPrint p
    pPrint (ArrayPattern ps) =
        brackets (mintercalate (text ", ") (fmap pPrint ps))

instance Pretty Name where
    pPrint (Name qs s) = mintercalate (text ".") (fmap pPrint (qs ++ [s]))

instance Pretty Id where
    pPrint (Id s _) = text (unpack s)

prettyEnv m = render (foldrWithKey (\k v r -> pPrint k <+> text ":" <+> pPrint v $$ r) mempty m)

fromString = fromText builtinLocation . pack

fromText l s = case fmap (flip Id l) (split (== '.') s) of
    [] -> error "Name from empty"
    is -> Name (init is) (last is)

fromId i = Name [] i

isConstructor i = isUpper (head (pretty i))

isOperator i = any isSym (pretty i)

-- these are not symbols in unicode, but in the language
-- otherwise e.g 2 - 2 would not be lexed as minus
isSym x = isSymbol x || elem x "!%&*/?@-:"

isUnqualified (Name [] _) = True
isUnqualified _ = False

builtinLocation = Location (Position 0 0) (Position 0 0) ""

makeId s = Id (pack s) builtinLocation

getId (Name _ i) = i

excluding xs = filter (flip notElem xs)

including xs = filter (flip elem xs)

getDefsD (ExpressionDeclaration p _) = getDefsP p
getDefsD (EnumDeclaration _ _ cs) = fmap fst cs
getDefsD _ = []

getDefsP p =
    let
        f (VariablePattern v) = [v]
        f (AliasPattern i _) = [i]
        f _ = []
    in concatMap f (universe p)

nNewVars n = fmap (\x -> makeId ("_v" ++ show x)) [1..n]

makeOp op a b =
    FunctionApplication (FunctionApplication
        (if isConstructor (getId op) then ConstructorExpression op else Variable op) a) b

makeOpPat op a b =
    ConstructorPattern op [a, b]

makeOpTyp op a b =
    TypeApplication (TypeApplication (TypeConstructor op) a) b

throwString s = Left [s]

fromEitherM (Left s) = fail (pretty s)
fromEitherM (Right r) = return r

find o m = mfind o m ()

mfind o m =
    maybe (fail ("Unknown " ++ pretty o ++ " in " ++ pretty (keys m))) return (lookup o m)

locationInfo (Id s l) = pretty l

locationInfoName (Name _ i) = locationInfo i

locationInfoP (VariablePattern v) = locationInfo v
locationInfoP (ConstructorPattern c ps) =
    "(" ++ locationInfoName c ++ mintercalate " " (fmap locationInfoP ps) ++ ")"
locationInfoP (ArrayPattern ps) =
    mintercalate " " (fmap locationInfoP ps)
locationInfoP other = pretty other

instance Monoid e => MonadPlus (Either e) where
    mzero = Left mempty
    mplus (Left sa) (Left sb) = Left (mappend sa sb)
    mplus (Left _) a = a
    mplus a _ = a

instance Monoid e => Alternative (Either e) where
    empty = mzero
    (<|>) = mplus
