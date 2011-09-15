module Parser where

import Text.Parsec
import Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import AST


javadef = emptyDef{commentStart = "",
                   commentEnd = "",
                   commentLine = "//",
                   identStart = letter,
                   identLetter = letter <|> alphaNum <|> char '_',
                   reservedNames = ["abstract","boolean",
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
          
javaTokenParser = makeTokenParser javadef                                    

name = T.identifier javaTokenParser
semi = T.

statements = semiSep statement

statement = varStatement

varStatement = commaSep1 aux
 where aux = do
         v <- name
         r <- option Nothing $ exp >>= return . Just
         return (v,r)
         
exp = undefined