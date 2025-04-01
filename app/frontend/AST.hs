{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
module AST where
import Data.List.NonEmpty (NonEmpty, last)
import Text.Gigaparsec (Parsec, (<**>))
import Text.Gigaparsec.Position (pos, Pos)
import Prelude hiding (GT, LT, EQ, last)
import Text.Gigaparsec.Errors.Combinator (filterOut, (<?>), label)

type Ident = String
type Dimension = Int

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
mkIntA = (pos <**>) . (IntA <$>)
mkBoolA :: Parsec Bool -> Parsec Atom
mkBoolA = (pos <**>) . (BoolA <$>)
mkCharA :: Parsec Char -> Parsec Atom
mkCharA = (pos <**>) . (CharA <$>)
mkStrA :: Parsec String -> Parsec Atom
mkStrA = (pos <**>) . (StrA <$>)
mkNullPairA :: Parsec Atom
mkNullPairA = NullPairA <$> pos
mkIdentA :: Parsec Ident -> Parsec Atom
mkIdentA = (pos <**>) . (IdentA <$>)
mkArrayElemA :: Parsec ArrayElem -> Parsec Atom
mkArrayElemA = (pos <**>) . (ArrayElemA <$>)
mkExprA :: Parsec Expr -> Parsec Atom
mkExprA = (pos <**>) . (ExprA <$>)

data ArrayElem = ArrayElem Ident [Expr] Pos
  deriving Show

mkArrayElem :: Parsec Ident -> Parsec [Expr] -> Parsec ArrayElem
mkArrayElem i ees = pos <**> (ArrayElem <$> i <*> ees) <?> ["array element"]

data UnaryOp = Not Pos | Neg Pos | Len Pos | Ord Pos | Chr Pos
  deriving Show

mkNot :: Parsec (Expr -> Expr)
mkNot = mkUnOpE <*> (Not <$> pos) <?> ["unary operator"]
mkNeg :: Parsec (Expr -> Expr)
mkNeg = mkUnOpE <*> (Neg <$> pos) <?> ["unary operator"]
mkLen :: Parsec (Expr -> Expr)
mkLen = mkUnOpE <*> (Len <$> pos) <?> ["unary operator"]
mkOrd :: Parsec (Expr -> Expr)
mkOrd = mkUnOpE <*> (Ord <$> pos) <?> ["unary operator"]
mkChr :: Parsec (Expr -> Expr)
mkChr = mkUnOpE <*> (Chr <$> pos) <?> ["unary operator"]

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
mkMul = mkBinOpE <*> (Mul <$> pos) <?> ["binary operator"]
mkDiv :: Parsec (Expr -> Expr -> Expr)
mkDiv = mkBinOpE <*> (Div <$> pos) <?> ["binary operator"]
mkMod :: Parsec (Expr -> Expr -> Expr)
mkMod = mkBinOpE <*> (Mod <$> pos) <?> ["binary operator"]
mkPlus :: Parsec (Expr -> Expr -> Expr)
mkPlus = mkBinOpE <*> (Plus <$> pos) <?> ["binary operator"]
mkMinus :: Parsec (Expr -> Expr -> Expr)
mkMinus = mkBinOpE <*> (Minus <$> pos) <?> ["binary operator"]
mkGT :: Parsec (Expr -> Expr -> Expr)
mkGT = mkBinOpE <*> (GT <$> pos) <?> ["binary operator"]
mkGTE :: Parsec (Expr -> Expr -> Expr)
mkGTE = mkBinOpE <*> (GTE <$> pos) <?> ["binary operator"]
mkLT :: Parsec (Expr -> Expr -> Expr)
mkLT = mkBinOpE <*> (LT <$> pos) <?> ["binary operator"]
mkLTE :: Parsec (Expr -> Expr -> Expr)
mkLTE = mkBinOpE <*> (LTE <$> pos) <?> ["binary operator"]
mkEQ :: Parsec (Expr -> Expr -> Expr)
mkEQ = mkBinOpE <*> (EQ <$> pos) <?> ["binary operator"]
mkNE :: Parsec (Expr -> Expr -> Expr)
mkNE = mkBinOpE <*> (NE <$> pos) <?> ["binary operator"]
mkAnd :: Parsec (Expr -> Expr -> Expr)
mkAnd = mkBinOpE <*> (And <$> pos) <?> ["binary operator"]
mkOr :: Parsec (Expr -> Expr -> Expr)
mkOr = mkBinOpE <*> (Or <$> pos) <?> ["binary operator"]

mkIdentOrArrayElem :: (Ident -> Pos -> t) -> (ArrayElem -> Pos -> t) -> Parsec (Ident -> [Expr] -> t)
mkIdentOrArrayElem identCon arrayCon
  = (\p i ees -> case ees of
      [] -> identCon i p
      (_:_) -> arrayCon (ArrayElem i ees p) p
    ) <$> pos <?> ["identifier", "array element"]

mkIdentAOrArrayElemA :: Parsec (Ident -> [Expr] -> Atom)
mkIdentAOrArrayElemA = mkIdentOrArrayElem IdentA ArrayElemA

-- Types
data Type = BaseT BaseType Pos | ArrayT ArrayType Pos | PairT PairType Pos
  deriving Show

mkBaseT :: Parsec BaseType -> Parsec Type
mkBaseT = label ["base type"] . (pos <**>) . (BaseT <$>)
mkArrayT :: Parsec ArrayType -> Parsec Type
mkArrayT = label ["array type"] . (pos <**>) . (ArrayT <$>)
mkPairT :: Parsec PairType -> Parsec Type
mkPairT = label ["pair type"] . (pos <**>) . (PairT <$>)

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
mkBasePE = (pos <**>) . (BasePE <$>)
mkArrayPE :: Parsec ArrayType -> Parsec PairElemType
mkArrayPE = (pos <**>) . (ArrayPE <$>)
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
  deriving Show

mkProg :: Parsec [Func] -> Parsec (NonEmpty Stmt) -> Parsec Program
mkProg ffs sss = pos <**> (Program <$> ffs <*> sss)

data Func = Func Type Ident [Param] (NonEmpty Stmt) Pos
  deriving Show

mkFunc :: Parsec (Type, Ident) -> Parsec [Param] -> Parsec (NonEmpty Stmt) -> Parsec Func
mkFunc ti pps sss = pos <**> (helper <$> ti <*> pps <*> sss')
  where
    sss' = filterOut (\s -> if lastStmtIsReturnOrExit (last s)
                         then Nothing
                         else Just "missing a return or exit on all exit paths"
                     ) sss
    lastStmtIsReturnOrExit = \case
      Return _ _        -> True
      Exit _ _          -> True
      If _ true false _ -> lastStmtIsReturnOrExit (last true) && lastStmtIsReturnOrExit (last false)
      While _ ws _      -> lastStmtIsReturnOrExit (last ws)
      Begin bs _        -> lastStmtIsReturnOrExit (last bs)
      _                 -> False

    helper (t, i) = Func t i

data Param = Param Type Ident Pos
  deriving Show

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
            If Expr (NonEmpty Stmt) (NonEmpty Stmt) Pos |
            While Expr (NonEmpty Stmt) Pos |
            Begin (NonEmpty Stmt) Pos
  deriving Show

mkSkip :: Parsec Stmt
mkSkip = Skip <$> pos
mkDecl :: Parsec Type -> Parsec Ident -> Parsec Rvalue -> Parsec Stmt
mkDecl t i r = pos <**> (Decl <$> t <*> i <*> r)
mkAsgn :: Parsec Lvalue -> Parsec Rvalue -> Parsec Stmt
mkAsgn l r = pos <**> (Asgn <$> l <*> r)
mkRead :: Parsec Lvalue -> Parsec Stmt
mkRead = (pos <**>) . (Read <$>)
mkFree :: Parsec Expr -> Parsec Stmt
mkFree = (pos <**>) . (Free <$>)
mkReturn :: Parsec Expr -> Parsec Stmt
mkReturn = (pos <**>) . (Return <$>)
mkExit :: Parsec Expr -> Parsec Stmt
mkExit = (pos <**>) . (Exit <$>)
mkPrint :: Parsec Expr -> Parsec Stmt
mkPrint = (pos <**>) . (Print <$>)
mkPrintln :: Parsec Expr -> Parsec Stmt
mkPrintln = (pos <**>) . (Println <$>)
mkIf :: Parsec Expr -> Parsec (NonEmpty Stmt) -> Parsec (NonEmpty Stmt) -> Parsec Stmt
mkIf b tts ffs = pos <**> (If <$> b <*> tts <*> ffs)
mkWhile :: Parsec Expr -> Parsec (NonEmpty Stmt) -> Parsec Stmt
mkWhile b sss = pos <**> (While <$> b <*> sss)
mkBegin :: Parsec (NonEmpty Stmt) -> Parsec Stmt
mkBegin = (pos <**>) . (Begin <$>)

data Lvalue = IdentL Ident Pos | ArrayElemL ArrayElem Pos | PairElemL PairElem Pos
  deriving Show

mkPairElemL :: Parsec PairElem -> Parsec Lvalue
mkPairElemL = (pos <**>) . (PairElemL <$>)

mkIdentLOrArrayElemL :: Parsec (Ident -> [Expr] -> Lvalue)
mkIdentLOrArrayElemL = mkIdentOrArrayElem IdentL ArrayElemL

data Rvalue = ExprR Expr Pos | ArrayLiterR ArrayLiter Pos | NewPairR Expr Expr Pos | PairElemR PairElem Pos | CallR Ident [Expr] Pos
  deriving Show

mkExprR :: Parsec Expr -> Parsec Rvalue
mkExprR = (pos <**>) . (ExprR <$>)
mkArrayLiterR :: Parsec ArrayLiter -> Parsec Rvalue
mkArrayLiterR = (pos <**>) . (ArrayLiterR <$>)
mkNewPairR :: Parsec Expr -> Parsec Expr -> Parsec Rvalue
mkNewPairR e1 e2 = pos <**> (NewPairR <$> e1 <*> e2)
mkPairElemR :: Parsec PairElem -> Parsec Rvalue
mkPairElemR = (pos <**>) . (PairElemR <$>)
mkCallR :: Parsec Ident -> Parsec [Expr] -> Parsec Rvalue
mkCallR i ees = pos <**> (CallR <$> i <*> ees)

data PairElem = Fst Lvalue Pos | Snd Lvalue Pos
  deriving Show

mkFst :: Parsec Lvalue -> Parsec PairElem
mkFst = (pos <**>) . (Fst <$>)
mkSnd :: Parsec Lvalue -> Parsec PairElem
mkSnd = (pos <**>) . (Snd <$>)

data ArrayLiter = ArrayLiter (NonEmpty Expr) Pos
  deriving Show

mkArrayLiter :: Parsec (NonEmpty Expr) -> Parsec ArrayLiter
mkArrayLiter = (pos <**>) . (ArrayLiter <$>)
