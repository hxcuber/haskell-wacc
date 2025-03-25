module TypeParser where
import AST (BaseType (BInt, BBool, BChar, BString), Type (..), ArrayType (ArrayType), PairType (PairType), PairElemType (BasePE, ArrayPE, PairPE))
import Text.Gigaparsec.Char (string)
import Text.Gigaparsec (($>), (<|>), Parsec, (<~>))
import Text.Gigaparsec.Combinator (count, option, count1)

typeParser :: Parsec Type
typeParser = typeOrArrayType ((BaseT <$> baseType) <|> (PairT <$> pairType))

baseType :: Parsec BaseType
baseType = string "int"    $> BInt    <|>
           string "bool"   $> BBool   <|>
           string "char"   $> BChar   <|>
           string "string" $> BString

typeOrArrayType :: Parsec Type -> Parsec Type
typeOrArrayType pt =
  disambiguator <$> (pt <~> count (string "[" *> string "]"))
    where
      disambiguator (t, d) = if d == 0 then t else ArrayT (ArrayType t d)

pairType :: Parsec PairType
pairType =
  disambiguator <$> ((string "pair" *> string "(" *> pairElemType) <~> (string "," *> pairElemType <* string ")"))
    where
      disambiguator (pet1, pet2) = PairType pet1 pet2

pairElemType :: Parsec PairElemType
pairElemType = (helper1 <$> typeOrArrayType (BaseT <$> baseType)) <|>
               (helper2 <$> (string "pair" *> option ((string "(" *> pairElemType <* string ",") <~> (pairElemType <* string ")") <~> count1 (string "[" *> string "]"))))
  where
    helper1 (BaseT bt) = BasePE bt
    helper1 (ArrayT at) = ArrayPE at
    -- never hit
    helper1 (PairT _)   = PairPE

    helper2 Nothing                  = PairPE
    helper2 (Just ((pet1, pet2), d)) = ArrayPE (ArrayType (PairT (PairType pet1 pet2)) d)


