{-# LANGUAGE OverloadedStrings #-}

module TypeParser where

import AST (ArrayType (ArrayType),BaseType, PairElemType (ArrayPE, BasePE, PairPE), PairType (PairType), Type (..), mkIntB, mkBoolB, mkCharB, mkStringB, mkTypeOrArrayType, mkBaseT, mkPairT, mkPairType, mkErasedPairOrPairArray)
import Text.Gigaparsec (Parsec, (<|>), (<~>))
import Text.Gigaparsec.Combinator (count, count1, option)
import Lexer ()

typeParser :: Parsec Type
typeParser = mkTypeOrArrayType <*> (mkBaseT baseType <|> mkPairT pairType) <*> count ("[" *> "]")

baseType :: Parsec BaseType
baseType = mkIntB    <* "int"    <|>
           mkBoolB   <* "bool"   <|>
           mkCharB   <* "char"   <|>
           mkStringB <* "string"

pairType :: Parsec PairType
pairType = mkPairType ("pair" *> "(" *> pairElemType) ("," *> pairElemType <* ")")

pairElemType :: Parsec PairElemType
pairElemType =
  helper <$> (mkTypeOrArrayType <*> mkBaseT baseType <*> count ("[" *> "]")) <|>
  mkErasedPairOrPairArray <*> option (("(" *> pairElemType <* ",") <~> (pairElemType <* ")") <~> count1 ("[" *> "]"))
    where
      helper (BaseT t p) = BasePE t p
      helper (ArrayT atype@(ArrayType {}) p') = ArrayPE atype p'
      -- never hit
      helper (PairT (PairType {}) p') = PairPE p'
