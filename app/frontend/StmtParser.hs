{-# LANGUAGE OverloadedStrings #-}
module StmtParser where

import Lexer (myIdentifier)
import TypeParser (typeParser)
import AST (mkParam, Param, mkIdentLOrArrayElemL, Lvalue, mkProg, Program, Stmt, Func, mkFunc)
import Text.Gigaparsec (Parsec, many, (<~>), atomic)
import ExprParser (expr)
import Text.Gigaparsec.Combinator.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import Text.Gigaparsec.Combinator (sepBy)


prog :: Parsec Program
prog = mkProg ("begin" *> many func) stmtList <* "end"

param :: Parsec Param
param = mkParam typeParser myIdentifier

lvalue :: Parsec Lvalue
lvalue = mkIdentLOrArrayElemL <*> myIdentifier <*> many ("[" *> expr <* "]")

stmtList :: Parsec (NonEmpty Stmt)
stmtList = sepBy1 stmt ","

func :: Parsec Func
func = mkFunc (atomic typeParser <~> myIdentifier <* "(" ) (sepBy param "," <* ")" <* "is") stmtList

stmt :: Parsec Stmt
stmt = undefined
