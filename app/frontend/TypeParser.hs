{-# LANGUAGE OverloadedStrings #-}

module TypeParser where

import AST (ArrayType (ArrayType), BaseType (BBool, BChar, BInt, BString), PairElemType (ArrayPE, BasePE, PairPE), PairType (PairType), Type (..))
import Text.Gigaparsec (Parsec, ($>), (<|>), (<~>))
import Text.Gigaparsec.Combinator (count, count1, option)
import Lexer ()

typeParser :: Parsec Type
typeParser = typeOrArrayType ((BaseT <$> baseType) <|> (PairT <$> pairType))

baseType :: Parsec BaseType
baseType = "int"    $> BInt    <|>
           "bool"   $> BBool   <|>
           "char"   $> BChar   <|>
           "string" $> BString

typeOrArrayType :: Parsec Type -> Parsec Type
typeOrArrayType pt =
  disambiguator <$> (pt <~> count ("[" *> "]"))
  where
    disambiguator (t, d) = if d == 0 then t else ArrayT (ArrayType t d)

pairType :: Parsec PairType
pairType =
  disambiguator <$> (("pair" *> "(" *> pairElemType) <~> ("," *> pairElemType <* ")"))
  where
    disambiguator (pet1, pet2) = PairType pet1 pet2

pairElemType :: Parsec PairElemType
pairElemType =
  (helper1 <$> typeOrArrayType (BaseT <$> baseType))
    <|> (helper2 <$> ("pair" *> option (("(" *> pairElemType <* ",") <~> (pairElemType <* ")") <~> count1 ("[" *> "]"))))
  where
    helper1 (BaseT bt) = BasePE bt
    helper1 (ArrayT at) = ArrayPE at
    -- never hit
    helper1 (PairT _) = PairPE

    helper2 Nothing = PairPE
    helper2 (Just ((pet1, pet2), d)) = ArrayPE (ArrayType (PairT (PairType pet1 pet2)) d)
