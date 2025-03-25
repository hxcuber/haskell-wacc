{-# LANGUAGE OverloadedLists #-}
module Lexer where
import Text.Gigaparsec.Token.Descriptions (plain, LexicalDesc (nameDesc, symbolDesc, textDesc, spaceDesc), plainName, NameDesc (..), plainSymbol, SymbolDesc (..), plainText, TextDesc (escapeSequences, graphicCharacter), plainEscape, EscapeDesc (..), plainSpace, SpaceDesc (lineCommentStart))
import Data.Char (isLetter, isDigit, ord)
import Text.Gigaparsec.Token.Lexer (mkLexerWithErrorConfig, Lexer (lexeme), decimal32, Names (identifier), Lexeme (names, stringLiteral, charLiteral), integer, TextParsers (ascii), apply, fully)
import Text.Gigaparsec.Token.Errors (defaultErrorConfig, ErrorConfig (labelSymbol), reason)
import qualified Text.Gigaparsec.Token.Lexer as Text.Gigaparsec.Token
import qualified Text.Gigaparsec
import Text.Gigaparsec (atomic, notFollowedBy)
import Text.Gigaparsec.Char (char, digit)

myLexicalDesc :: Text.Gigaparsec.Token.Descriptions.LexicalDesc
myLexicalDesc = plain
  {
    nameDesc = myNameDesc,
    symbolDesc = mySymbolDesc,
    textDesc = myTextDesc,
    spaceDesc = mySpaceDesc
  }

myNameDesc :: Text.Gigaparsec.Token.Descriptions.NameDesc
myNameDesc = plainName
  { identifierStart = Just (\c -> isLetter c || c == '_'),
    identifierLetter = Just (\c -> isLetter c || isDigit c || c == '_')
  }

mySymbolDesc :: Text.Gigaparsec.Token.Descriptions.SymbolDesc
mySymbolDesc = plainSymbol
  {
    hardKeywords =
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

myTextDesc :: Text.Gigaparsec.Token.Descriptions.TextDesc
myTextDesc = plainText
  {
    escapeSequences = plainEscape
      {
        literals = ['\\', '"', '\''],
        mapping  =
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

mySpaceDesc :: Text.Gigaparsec.Token.Descriptions.SpaceDesc
mySpaceDesc = plainSpace
  { lineCommentStart = "#"
  }

myErrorConfig :: Text.Gigaparsec.Token.Errors.ErrorConfig
myErrorConfig = defaultErrorConfig
  {
    labelSymbol =
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

myLexer :: Text.Gigaparsec.Token.Lexer.Lexer
myLexer = mkLexerWithErrorConfig myLexicalDesc myErrorConfig

myLexeme :: Text.Gigaparsec.Token.Lexeme
myLexeme = lexeme myLexer

myIdentifier :: Text.Gigaparsec.Parsec String
myIdentifier = identifier $ names myLexeme

myInteger :: Text.Gigaparsec.Parsec Int
myInteger = decimal32 $ integer myLexeme

myString :: Text.Gigaparsec.Parsec String
myString = ascii $ stringLiteral myLexeme

myChar :: Text.Gigaparsec.Parsec Char
myChar = ascii $ charLiteral myLexeme

negUnary :: Text.Gigaparsec.Parsec ()
negUnary = apply myLexeme (atomic (char '-' *> notFollowedBy digit))

myFully :: Text.Gigaparsec.Parsec a -> Text.Gigaparsec.Parsec a
myFully = fully myLexer
