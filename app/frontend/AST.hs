module AST where
import Data.List.NonEmpty (NonEmpty)

type Ident = String
type Dimension = Int

-- Expressions
data Expr = UnOp UnaryOp Expr |
            BinOp Expr BinOp Expr |
            Atom Atom
data Atom = Int Int | Bool Bool | Char Char | Str String | NullPair | Ident Ident | ArrElem ArrayElem | Expr Expr
data ArrayElem = ArrayElem Ident [Expr]
data UnaryOp = Not | Neg | Len | Ord | Chr
data BinOp = Mul | Div | Mod | Plus | Minus | GT | GTE | LT | LTE | EQ | NE | And | Or

-- Types
data Type = BaseT BaseType | ArrayT ArrayType | PairT PairType
  deriving Show
data BaseType = BInt | BBool | BChar | BString
  deriving Show
data ArrayType = ArrayType Type Dimension
  deriving Show
data PairType = PairType PairElemType PairElemType
  deriving Show
data PairElemType = BasePE BaseType | ArrayPE ArrayType | PairPE
  deriving Show

-- Statements
data Program = Program (NonEmpty Func) (NonEmpty Stmt)
data Func = Func Type Ident [Param] (NonEmpty Stmt)
data Param = Param Type Ident
data Stmt = Skip |
            Decl Type Ident Rvalue |
            Asgn Lvalue Rvalue |
            Read Lvalue |
            Free Expr |
            Return Expr |
            Exit Expr |
            Print Expr |
            Println Expr |
            If Expr Stmt Stmt |
            While Expr Stmt |
            Begin Stmt
data Lvalue = IdentL Ident | ArrayElemL ArrayElem | PairElemL PairElem
data Rvalue = ExprR Expr | ArrayLiterR ArrayLiter | NewPairR Expr Expr | PairElemR PairElem | CallR Ident [Expr]
data PairElem = Fst Lvalue | Snd Lvalue
newtype ArrayLiter = ArrayLiter [Expr]
