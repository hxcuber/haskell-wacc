{-# LANGUAGE OverloadedStrings #-}
module ExprParser where
import Lexer (myInteger, myChar, myString, negUnary, myIdentifier)
import AST (mkIntA, mkBoolA, Atom, mkCharA, mkStrA, mkNullPairA, mkExprA, mkOr, mkAtomE, Expr, mkAnd, mkNot, mkNeg, mkLen, mkOrd, mkChr, mkMul, mkMod, mkDiv, mkPlus, mkMinus, mkEQ, mkGT, mkLT, mkNE, mkIdentOrArrayElem)
import Text.Gigaparsec ((<|>), ($>), Parsec, many)
import Text.Gigaparsec.Errors.Combinator (hide)
import Text.Gigaparsec.Expr (precedence, sops, (+<), Fixity (InfixR, Prefix, InfixL, InfixN), Prec (Atom))

expr :: Parsec Expr
expr = precedence $
       sops Prefix [mkNot <* "!", mkNeg <* negUnary, mkLen <* "len", mkOrd <* "ord", mkChr <* "chr"] +<
       sops InfixL [mkMul <* "*", mkMod <* "%", mkDiv <* "/"]                                        +<
       sops InfixL [mkPlus <* "+", mkMinus <* "-"]                                                   +<
       sops InfixN [mkGT <* ">", mkLT <* "<", mkGT <* ">=", mkLT <* "<="]                            +<
       sops InfixN [mkEQ <* "==", mkNE <* "!="]                                                      +<
       sops InfixR [mkAnd <* "&&"]                                                                   +<
       sops InfixR [mkOr <* "||"]                                                                    +<
       Atom (mkAtomE <*> atom)

atom :: Parsec Atom
atom = mkIntA  myInteger                                                 <|>
       mkBoolA ("true" $> True <|> "false" $> False)                     <|>
       mkCharA myChar                                                    <|>
       mkStrA myString                                                   <|>
       mkNullPairA <* "null"                                             <|>
       mkIdentOrArrayElem <*> myIdentifier <*> many ("[" *> expr <* "]") <|>
       hide (mkExprA ("(" *> expr <* ")"))

