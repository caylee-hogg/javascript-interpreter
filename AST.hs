module AST where

import Data.Either

type Refine = Either String Exp

data Statement = VarStatement [(String, Maybe Exp)]
               | Statements Statements

type Statements = [(Maybe String, LStatement)]

data LStatement = Exp Exp
                | Disrupt Disrupter
                | IfStmt Exp Statements Statements
                -- put switch in
                | While Exp Statements
                | ForIn String Exp Statements
                | For (Maybe Exp) (Maybe Exp) (Maybe Exp) Statements
                | Do Statements Exp

data Disrupter = Break (Maybe String)
               | Return Exp
               | Throw Exp

data Exp = NumLit Double
         | StrLit String
         | ObjLit [(String,Exp)]
         | Var String
         | Unop Unop Exp
         | Binop Exp Binop Exp
         | Refine Exp Refine
         | Application Exp [Exp]
         | New Exp [Exp]
         | Delete Exp Refine

data Unop = TypeOf
          | ToNum
          | Negate
          | Not
            
data Binop = Times | Div | Mod | Add | Sub | GEq | LEq | GE | LE | Eq | Or | And
