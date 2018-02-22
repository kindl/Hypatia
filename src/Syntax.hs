{-# LANGUAGE DeriveDataTypeable #-}
module Syntax where

import Prelude hiding (lookup)
import Data.Data
import Data.Maybe(fromMaybe)
import Data.Text(Text, unpack, pack, split)
import Data.Char(isUpper, isSymbol)
import Data.Functor.Identity(runIdentity)
import Data.Hashable(Hashable, hashWithSalt)
import Data.HashMap.Strict(lookup, keys, foldrWithKey, filterWithKey)
import Text.PrettyPrint.HughesPJClass(Pretty, pPrint)
import Text.PrettyPrint(text, (<+>), ($$), (<>), parens, brackets, render)
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

data Name = Name [Text] Id
    deriving (Show, Data, Typeable, Eq)

instance Hashable Name where
    hashWithSalt s (Name a b) = hashWithSalt s (a, b)


data ModuleDeclaration =
    ModuleDeclaration Name [Declaration]
        deriving (Show, Data, Typeable)

getDecls (ModuleDeclaration _ decls) = decls
getName (ModuleDeclaration name _) = name


data Literal
    = Numeral Double
    | Text Text
        deriving (Show, Data, Typeable)


data Associativity = None | LeftAssociative | RightAssociative
    deriving (Show, Data, Typeable)

type Precedence = Integer
type Alias = Id

data Declaration
    = ImportDeclaration Name (Maybe [Id]) (Maybe Name)
    | TypeDeclaration Id [Id] [(Id, [Type])]
    | ExpressionDeclaration Pattern Expression
    | AliasDeclaration Id Type
    -- simplified to an expression declaration
    | FunctionDeclaration Id [Pattern] Expression
    | FixityDeclaration Associativity Precedence Id Alias
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
        deriving (Show, Data, Typeable)


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
-- TODO think about a nicer way to add case lambdas
-- It should have a way to match on more than just a single value
-- The functions could be easily turned into case lambdas as well
-- Without the use of tuples
--    | CaseLambdaExpression [(Pattern, Expression)]
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

qualify (Name modQs modName) (Name [] name) = Name (modQs ++ [getText modName]) name
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
    pPrint = flatName (text ".") (text ".")

instance Pretty Id where
    pPrint (Id s _) = text (unpack s)

-- displays the module name joined with underscore
flatVar = flatName (text "_") (text ".")

flatModName = flatName (text "_") (text "_")

flatName _ _ (Name [] s) = pPrint s
flatName qualSep idSep (Name qs s) =
    mintercalate qualSep (fmap (text . unpack) qs) <> idSep <> pPrint s

prettyEnv m = render (foldrWithKey (\k v r -> pPrint k <+> text ":" <+> pPrint v $$ r) mempty m)

fromString = fromText builtinLocation . pack

fromText l s =
    let is = split (== '.') s
    in Name (init is) (Id (last is) l)

fromId = Name []

isConstructor i = isUpper (head (pretty i))

isOperator i = any isSym (pretty i)

-- these are not symbols in unicode, but in the language
-- otherwise e.g 2 - 2 would not be lexed as minus
isSym x = isSymbol x || elem x "!%&*/?@-:"

isUnqualified (Name [] _) = True
isUnqualified _ = False

builtinLocation = Location (Position 0 0) (Position 0 0) ""

makeId s = Id (pack s) builtinLocation

getQualifiers (Name q _) = q

getId (Name _ i) = i

getText (Id t _) = t

excluding xs = filter (flip notElem xs)

including xs = filter (flip elem xs)

excludingKeys xs = filterWithKey (const . flip notElem xs)

includingKeys xs = filterWithKey (const . flip elem xs)

getDefsD (ExpressionDeclaration p _) = getDefsP p
getDefsD (TypeDeclaration _ _ cs) = fmap fst cs
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

fromEitherM (Left s) = fail s
fromEitherM (Right r) = return r

find o m = runIdentity (mfind o m)

mfind o m =
    maybe (fail ("Unknown " ++ prettyWithInfo o ++ " in " ++ pretty (keys m))) return (lookup o m)

findId i m =
    fromMaybe (error ("Unknown " ++ pretty i ++ " " ++ locationInfo i ++ " in " ++ pretty (keys m))) (lookup i m)

prettyWithInfo n = pretty n ++ " " ++ locationInfo (getId n)

locationInfo (Id _ l) = pretty l

locationInfoName (Name _ i) = locationInfo i

locationInfoP (VariablePattern v) = locationInfo v
locationInfoP (ConstructorPattern c ps) =
    "(" ++ locationInfoName c ++ mintercalate " " (fmap locationInfoP ps) ++ ")"
locationInfoP (ArrayPattern ps) =
    mintercalate " " (fmap locationInfoP ps)
locationInfoP other = pretty other
