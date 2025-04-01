{-# LANGUAGE OverloadedStrings #-}
module StmtParser where

import Lexer (myIdentifier, myFully)
import TypeParser (typeParser)
import AST (mkParam, Param, mkIdentLOrArrayElemL, Lvalue, mkProg, Program, Stmt, Func, mkFunc, Rvalue, mkSkip, mkDecl, mkAsgn, mkRead, mkFree, mkReturn, mkExit, mkPrint, mkPrintln, mkIf, mkWhile, mkBegin, mkPairElemL, mkSnd, mkFst, PairElem, mkExprR, mkArrayLiterR, mkNewPairR, mkPairElemR, mkCallR, mkArrayLiter, ArrayLiter)
import Text.Gigaparsec (Parsec, many, (<~>), atomic, (<|>))
import ExprParser (expr)
import Text.Gigaparsec.Combinator.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import Text.Gigaparsec.Combinator (sepBy)


prog :: Parsec Program
prog = myFully $ mkProg ("begin" *> many func) stmtList <* "end"

param :: Parsec Param
param = mkParam typeParser myIdentifier

stmtList :: Parsec (NonEmpty Stmt)
stmtList = sepBy1 stmt ","

func :: Parsec Func
func = mkFunc (atomic typeParser <~> myIdentifier <* "(" ) (sepBy param "," <* ")" <* "is") stmtList <* "end"

stmt :: Parsec Stmt
stmt = mkSkip <* "skip"                                                           <|>
       mkDecl    typeParser myIdentifier ("=" *> rvalue)                          <|>
       mkAsgn    lvalue ("=" *> rvalue)                                           <|>
       mkRead    ("read" *> lvalue)                                               <|>
       mkFree    ("free" *> expr)                                                 <|>
       mkReturn  ("return" *> expr)                                               <|>
       mkExit    ("exit" *> expr)                                                 <|>
       mkPrint   ("print" *> expr)                                                <|>
       mkPrintln ("println" *> expr)                                              <|>
       mkIf      ("if" *> expr) ("then" *> stmtList) ("else" *> stmtList <* "fi") <|>
       mkWhile   ("while" *> expr) ("do" *> stmtList <* "done")                   <|>
       mkBegin   ("begin" *> stmtList <* "end")

lvalue :: Parsec Lvalue
lvalue = mkIdentLOrArrayElemL <*> myIdentifier <*> many ("[" *> expr <* "]") <|> mkPairElemL pairElem

rvalue :: Parsec Rvalue
rvalue = mkExprR expr <|>
         mkArrayLiterR arrayLiter <|>
         mkNewPairR ("(" *> expr) ("," *> expr <* ")") <|>
         mkPairElemR pairElem <|>
         mkCallR ("call" *> myIdentifier) ("(" *> sepBy expr "," <* ")")

pairElem :: Parsec PairElem
pairElem = mkFst ("fst" *> lvalue) <|> mkSnd ("snd" *> lvalue)

arrayLiter :: Parsec ArrayLiter
arrayLiter = mkArrayLiter ("[" *> sepBy1 expr "," <* "]")