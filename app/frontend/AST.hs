module AST where
import Data.List.NonEmpty (NonEmpty)
import Text.Gigaparsec (Parsec, (<**>))
import Text.Gigaparsec.Position (pos, Pos)
import Prelude hiding (GT, LT, EQ)

type Ident = String
type Dimension = Int
type Row = Int
type Col = Int

-- Expressions
data Expr = UnOpE UnaryOp Expr Pos |
            BinOpE Expr BinOp Expr Pos |
            AtomE Atom Pos
  deriving Show
mkUnOpE :: Parsec (UnaryOp -> Expr -> Expr)
mkUnOpE = (\p u e -> UnOpE u e p) <$> pos
mkBinOpE :: Parsec (Expr -> BinOp -> Expr -> Expr)
mkBinOpE = (\p e1 b e2 -> BinOpE e1 b e2 p) <$> pos
mkAtomE :: Parsec (Atom -> Expr)
mkAtomE = flip AtomE <$> pos

data Atom = IntA Int Pos |
            BoolA Bool Pos |
            CharA Char Pos |
            StrA String Pos |
            NullPairA Pos |
            IdentA Ident Pos |
            ArrayElemA ArrayElem Pos |
            ExprA Expr Pos
  deriving Show
mkIntA :: Parsec Int -> Parsec Atom
mkIntA i = pos <**> (IntA <$> i)
mkBoolA :: Parsec Bool -> Parsec Atom
mkBoolA b = pos <**> (BoolA <$> b)
mkCharA :: Parsec Char -> Parsec Atom
mkCharA c = pos <**> (CharA <$> c)
mkStrA :: Parsec String -> Parsec Atom
mkStrA s = pos <**> (StrA <$> s)
mkNullPairA :: Parsec Atom
mkNullPairA = NullPairA <$> pos
mkIdentA :: Parsec Ident -> Parsec Atom
mkIdentA i = pos <**> (IdentA <$> i)
mkArrayElemA :: Parsec ArrayElem -> Parsec Atom
mkArrayElemA ae = pos <**> (ArrayElemA <$> ae)
-- susge
mkExprA :: Parsec Expr -> Parsec Atom
mkExprA e = pos <**> (ExprA <$> e)

data ArrayElem = ArrayElem Ident [Expr] Pos
  deriving Show
mkArrayElem :: Parsec Ident -> Parsec [Expr] -> Parsec ArrayElem
mkArrayElem i ees = pos <**> (ArrayElem <$> i <*> ees)

data UnaryOp = Not Pos | Neg Pos | Len Pos | Ord Pos | Chr Pos
  deriving Show
mkNot :: Parsec UnaryOp
mkNot = Not <$> pos
mkNeg :: Parsec UnaryOp
mkNeg = Neg <$> pos
mkLen :: Parsec UnaryOp
mkLen = Len <$> pos
mkOrd :: Parsec UnaryOp
mkOrd = Ord <$> pos
mkChr :: Parsec UnaryOp
mkChr = Chr <$> pos

data BinOp = Mul Pos |
             Div Pos |
             Mod Pos |
             Plus Pos |
             Minus Pos |
             GT Pos |
             GTE Pos |
             LT Pos |
             LTE Pos |
             EQ Pos |
             NE Pos |
             And Pos |
             Or Pos
  deriving Show
mkMul :: Parsec BinOp
mkMul = Mul <$> pos
mkDiv :: Parsec BinOp
mkDiv = Div <$> pos
mkMod :: Parsec BinOp
mkMod = Mod <$> pos
mkPlus :: Parsec BinOp
mkPlus = Plus <$> pos
mkMinus :: Parsec BinOp
mkMinus = Minus <$> pos
mkGT :: Parsec BinOp
mkGT = GT <$> pos
mkGTE :: Parsec BinOp
mkGTE = GTE <$> pos
mkLT :: Parsec BinOp
mkLT = LT <$> pos
mkLTE :: Parsec BinOp
mkLTE = LTE <$> pos
mkEQ :: Parsec BinOp
mkEQ = EQ <$> pos
mkNE :: Parsec BinOp
mkNE = NE <$> pos
mkAnd :: Parsec BinOp
mkAnd = And <$> pos
mkOr :: Parsec BinOp
mkOr = Or <$> pos

-- Types
data Type = BaseT BaseType Pos | ArrayT ArrayType Pos | PairT PairType Pos
  deriving Show
mkBaseT :: Parsec BaseType -> Parsec Type
mkBaseT bt = pos <**> (BaseT <$> bt)
mkArrayT :: Parsec ArrayType -> Parsec Type
mkArrayT at = pos <**> (ArrayT <$> at)
mkPairT :: Parsec PairType -> Parsec Type
mkPairT pt  = pos <**> (PairT <$> pt)

data BaseType = IntB Pos | BoolB Pos | CharB Pos | StringB Pos
  deriving Show

mkIntB :: Parsec BaseType
mkIntB = IntB <$> pos
mkBoolB :: Parsec BaseType
mkBoolB = BoolB <$> pos
mkCharB :: Parsec BaseType
mkCharB = CharB <$> pos
mkStringB :: Parsec BaseType
mkStringB = StringB <$> pos

data ArrayType = ArrayType Type Dimension Pos
  deriving Show
mkArrayType :: Parsec Type -> Parsec Dimension -> Parsec ArrayType
mkArrayType t d = pos <**> (ArrayType <$> t <*> d)

mkTypeOrArrayType :: Parsec (Type -> Dimension -> Type)
mkTypeOrArrayType = (\p t d -> if d == 0 then t else ArrayT (ArrayType t d p) p) <$> pos

data PairType = PairType PairElemType PairElemType Pos
  deriving Show
mkPairType :: Parsec PairElemType -> Parsec PairElemType -> Parsec PairType
mkPairType pet1 pet2 = pos <**> (PairType <$> pet1 <*> pet2)

data PairElemType = BasePE BaseType Pos | ArrayPE ArrayType Pos | PairPE Pos
  deriving Show
mkBasePE :: Parsec BaseType -> Parsec PairElemType
mkBasePE bt = pos <**> (BasePE <$> bt)
mkArrayPE :: Parsec ArrayType -> Parsec PairElemType
mkArrayPE at = pos <**> (ArrayPE <$> at)
mkPairPE :: Parsec PairElemType
mkPairPE = PairPE <$> pos

mkErasedPairOrPairArray :: Parsec (Maybe ((PairElemType, PairElemType), Dimension) -> PairElemType)
mkErasedPairOrPairArray = (\p o ->
  case o of
    Nothing                  -> PairPE p
    (Just ((pet1, pet2), d)) -> ArrayPE (ArrayType (PairT (PairType pet1 pet2 p) p) d p) p
  ) <$> pos

-- Statements
data Program = Program [Func] (NonEmpty Stmt) Pos
data Func = Func Type Ident [Param] (NonEmpty Stmt) Pos
data Param = Param Type Ident Pos
data Stmt = Skip Pos |
            Decl Type Ident Rvalue Pos|
            Asgn Lvalue Rvalue Pos |
            Read Lvalue Pos |
            Free Expr Pos |
            Return Expr Pos |
            Exit Expr Pos |
            Print Expr Pos |
            Println Expr Pos |
            If Expr Stmt Stmt Pos |
            While Expr Stmt Pos |
            Begin Stmt Pos
data Lvalue = IdentL Ident Pos | ArrayElemL ArrayElem Pos | PairElemL PairElem Pos
data Rvalue = ExprR Expr Pos | ArrayLiterR ArrayLiter Pos | NewPairR Expr Expr Pos | PairElemR PairElem Pos | CallR Ident [Expr] Pos
data PairElem = Fst Lvalue Pos | Snd Lvalue Pos
data ArrayLiter = ArrayLiter [Expr] Pos
