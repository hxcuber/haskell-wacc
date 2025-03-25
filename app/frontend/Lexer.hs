{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lexer where

import Data.Char (isDigit, isLetter, ord)
import Text.Gigaparsec (Parsec, atomic, notFollowedBy)
import Text.Gigaparsec.Char (char, digit)
import Text.Gigaparsec.Token.Descriptions (EscapeDesc (..), LexicalDesc (..), NameDesc (..), SpaceDesc (..), SymbolDesc (..), TextDesc (..), plain, plainEscape, plainName, plainSpace, plainSymbol, plainText)
import Text.Gigaparsec.Token.Errors (ErrorConfig (..), ExplainConfigurable (reason), defaultErrorConfig)
import Text.Gigaparsec.Token.Lexer (Lexeme (..), Lexer, ascii, decimal32, fully, identifier, lexeme, mkLexerWithErrorConfig, names)
import Text.Gigaparsec.Token.Patterns (overloadedStrings)

myLexicalDesc :: LexicalDesc
myLexicalDesc =
  plain
    { nameDesc = myNameDesc,
      symbolDesc = mySymbolDesc,
      textDesc = myTextDesc,
      spaceDesc = mySpaceDesc
    }

myNameDesc :: NameDesc
myNameDesc =
  plainName
    { identifierStart = Just (\c -> isLetter c || c == '_'),
      identifierLetter = Just (\c -> isLetter c || isDigit c || c == '_')
    }

mySymbolDesc :: SymbolDesc
mySymbolDesc =
  plainSymbol
    { hardKeywords =
        [ "true",
          "false",
          "null",
          "int",
          "bool",
          "char",
          "string",
          "pair",
          "begin",
          "end",
          "is",
          "skip",
          "read",
          "free",
          "return",
          "exit",
          "print",
          "println",
          "if",
          "then",
          "else",
          "fi",
          "while",
          "do",
          "done",
          "newpair",
          "call",
          "len",
          "ord",
          "chr",
          "fst",
          "snd"
        ],
      hardOperators =
        [ "!",
          "-",
          "*",
          "/",
          "%",
          "+",
          "-",
          "<",
          "<=",
          ">",
          ">=",
          "==",
          "!=",
          "&&",
          "||",
          "="
        ]
    }

myTextDesc :: TextDesc
myTextDesc =
  plainText
    { escapeSequences =
        plainEscape
          { literals = ['\\', '"', '\''],
            mapping =
              [ ("0", '\0'),
                ("b", '\b'),
                ("t", '\t'),
                ("n", '\n'),
                ("f", '\f'),
                ("r", '\r')
              ]
          },
      graphicCharacter = Just (\c -> c /= '"' && c /= '\\' && c /= '\'' && ord c >= 0x20)
    }

mySpaceDesc :: SpaceDesc
mySpaceDesc =
  plainSpace
    { lineCommentStart = "#"
    }

myErrorConfig :: ErrorConfig
myErrorConfig =
  defaultErrorConfig
    { labelSymbol =
        [ ("!", reason "unary operator"),
          ("*", reason "arithmetic operator"),
          ("/", reason "arithmetic operator"),
          ("%", reason "arithmetic operator"),
          ("+", reason "arithmetic operator"),
          ("-", reason "arithmetic operator"),
          ("<", reason "boolean operator"),
          ("<=", reason "boolean operator"),
          (">", reason "boolean operator"),
          (">=", reason "boolean operator"),
          ("==", reason "boolean operator"),
          ("!=", reason "boolean operator"),
          ("&&", reason "boolean operator"),
          ("||", reason "boolean operator"),
          ("=", reason "arithmetic operator")
        ]
    }

myLexer :: Lexer
myLexer = mkLexerWithErrorConfig myLexicalDesc myErrorConfig

myLexeme :: Lexeme
myLexeme = lexeme myLexer

myIdentifier :: Parsec String
myIdentifier = identifier $ names myLexeme

myInteger :: Parsec Int
myInteger = decimal32 $ integer myLexeme

myString :: Parsec String
myString = ascii $ stringLiteral myLexeme

myChar :: Parsec Char
myChar = ascii $ charLiteral myLexeme

negUnary :: Parsec ()
negUnary = apply myLexeme (atomic (char '-' *> notFollowedBy digit))

myFully :: Parsec a -> Parsec a
myFully = fully myLexer

symLexeme :: String -> Parsec ()
symLexeme = sym myLexeme

overloadedStrings [|myLexer|]
