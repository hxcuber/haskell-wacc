{-# LANGUAGE LambdaCase #-}
module AST where
import Data.List.NonEmpty (NonEmpty, last)
import Text.Gigaparsec (Parsec, (<**>))
import Text.Gigaparsec.Position (pos, Pos)
import Prelude hiding (GT, LT, EQ, last)
import Text.Gigaparsec.Errors.Combinator (filterOut)

type Ident = String
type Dimension = Int
type Row = Int
type Col = Int

-- Expressions
data Expr = UnOpE UnaryOp Expr Pos |
            BinOpE BinOp Expr Expr Pos |
            AtomE Atom Pos
  deriving Show
mkUnOpE :: Parsec (UnaryOp -> Expr -> Expr)
mkUnOpE = (\p u e -> UnOpE u e p) <$> pos
mkBinOpE :: Parsec (BinOp -> Expr -> Expr -> Expr)
mkBinOpE = (\p b e1 e2 -> BinOpE b e1 e2 p) <$> pos
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
mkNot :: Parsec (Expr -> Expr)
mkNot = mkUnOpE <*> (Not <$> pos)
mkNeg :: Parsec (Expr -> Expr)
mkNeg = mkUnOpE <*> (Neg <$> pos)
mkLen :: Parsec (Expr -> Expr)
mkLen = mkUnOpE <*> (Len <$> pos)
mkOrd :: Parsec (Expr -> Expr)
mkOrd = mkUnOpE <*> (Ord <$> pos)
mkChr :: Parsec (Expr -> Expr)
mkChr = mkUnOpE <*> (Chr <$> pos)

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
mkMul :: Parsec (Expr -> Expr -> Expr)
mkMul = mkBinOpE <*> (Mul <$> pos)
mkDiv :: Parsec (Expr -> Expr -> Expr)
mkDiv = mkBinOpE <*> (Div <$> pos)
mkMod :: Parsec (Expr -> Expr -> Expr)
mkMod = mkBinOpE <*> (Mod <$> pos)
mkPlus :: Parsec (Expr -> Expr -> Expr)
mkPlus = mkBinOpE <*> (Plus <$> pos)
mkMinus :: Parsec (Expr -> Expr -> Expr)
mkMinus = mkBinOpE <*> (Minus <$> pos)
mkGT :: Parsec (Expr -> Expr -> Expr)
mkGT = mkBinOpE <*> (GT <$> pos)
mkGTE :: Parsec (Expr -> Expr -> Expr)
mkGTE = mkBinOpE <*> (GTE <$> pos)
mkLT :: Parsec (Expr -> Expr -> Expr)
mkLT = mkBinOpE <*> (LT <$> pos)
mkLTE :: Parsec (Expr -> Expr -> Expr)
mkLTE = mkBinOpE <*> (LTE <$> pos)
mkEQ :: Parsec (Expr -> Expr -> Expr)
mkEQ = mkBinOpE <*> (EQ <$> pos)
mkNE :: Parsec (Expr -> Expr -> Expr)
mkNE = mkBinOpE <*> (NE <$> pos)
mkAnd :: Parsec (Expr -> Expr -> Expr)
mkAnd = mkBinOpE <*> (And <$> pos)
mkOr :: Parsec (Expr -> Expr -> Expr)
mkOr = mkBinOpE <*> (Or <$> pos)

mkIdentOrArrayElem :: (Ident -> Pos -> t) -> (ArrayElem -> Pos -> t) -> Parsec (Ident -> [Expr] -> t)
mkIdentOrArrayElem identCon arrayCon
  = (\p i ees -> case ees of
      [] -> identCon i p
      (_:_) -> arrayCon (ArrayElem i ees p) p
    ) <$> pos

mkIdentAOrArrayElemA :: Parsec (Ident -> [Expr] -> Atom)
mkIdentAOrArrayElemA = mkIdentOrArrayElem IdentA ArrayElemA

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
mkErasedPairOrPairArray
  = (\p o -> case o of
      Nothing                  -> PairPE p
      (Just ((pet1, pet2), d)) -> ArrayPE (ArrayType (PairT (PairType pet1 pet2 p) p) d p) p
    ) <$> pos

-- Statements
data Program = Program [Func] (NonEmpty Stmt) Pos
mkProg :: Parsec [Func] -> Parsec (NonEmpty Stmt) -> Parsec Program
mkProg ffs sss = pos <**> (Program <$> ffs <*> sss)

data Func = Func Type Ident [Param] (NonEmpty Stmt) Pos
mkFunc :: Parsec (Type, Ident) -> Parsec [Param] -> Parsec (NonEmpty Stmt) -> Parsec Func
mkFunc ti pps sss = pos <**> (Func <$> t <*> i <*> pps <*> sss')
  where
    sss' = filterOut (\s -> if lastStmtIsReturnOrExit (last s)
                         then Nothing
                         else Just "missing a return or exit on all exit paths"
                     ) sss
    lastStmtIsReturnOrExit = \case
      Return _ _        -> True
      Exit _ _          -> True
      If _ true false _ -> lastStmtIsReturnOrExit true && lastStmtIsReturnOrExit false
      While _ ws _      -> lastStmtIsReturnOrExit ws
      Begin bs _        -> lastStmtIsReturnOrExit bs
      _                 -> False

data Param = Param Type Ident Pos
mkParam :: Parsec Type -> Parsec Ident -> Parsec Param
mkParam t i = pos <**> (Param <$> t <*> i)

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

mkIdentLOrArrayElemL :: Parsec (Ident -> [Expr] -> Lvalue)
mkIdentLOrArrayElemL = mkIdentOrArrayElem IdentL ArrayElemL

data Rvalue = ExprR Expr Pos | ArrayLiterR ArrayLiter Pos | NewPairR Expr Expr Pos | PairElemR PairElem Pos | CallR Ident [Expr] Pos
data PairElem = Fst Lvalue Pos | Snd Lvalue Pos
data ArrayLiter = ArrayLiter [Expr] Pos
