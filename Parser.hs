module Parser where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import AST


javadef = emptyDef{T.commentStart = "",
                   T.commentEnd = "",
                   T.commentLine = "//",
                   T.identStart = letter,
                   T.identLetter = letter <|> alphaNum <|> char '_',
                   T.reservedNames = ["abstract","boolean",
                                      "break", "byte",
                                      "case", "catch",
                                      "char", "class",
                                      "const", "continue",
                                      "debugger", "default",
                                      "delete", "do", "double",
                                      "else", "enum", "export",
                                      "extends", "false", "final",
                                      "finally", "float", "for",
                                      "function", "goto",
                                      "if", "implements", "import",
                                      "in", "instanceof", "int",
                                      "interface", "long", "native",
                                      "new", "null", "package",
                                      "private", "protected", "public",
                                      "return", "short", "static",
                                      "super", "switch", "synchronized",
                                      "this", "throw", "throws", "transient",
                                      "true", "try", "typeof", "var",
                                      "volatile", "void", "while", "with"]}
          
javaTokenParser = T.makeTokenParser javadef                                    

name = T.identifier javaTokenParser
semiSep = T.semiSep javaTokenParser
commaSep1 = T.commaSep1 javaTokenParser

statements = semiSep statement

statement = varStatement

varStatement = commaSep1 aux
 where aux = do
         v <- name
         r <- option Nothing $ expr >>= return . Just
         return (v,r)
         
-- need to use an expression parser here, built from the parsec machinery
expr = undefined