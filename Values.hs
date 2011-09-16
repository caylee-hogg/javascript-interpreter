module Values where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import AST

data JValue = JDouble Double
            | JString String
            | JObject Int [(String,JValue)]
            | JFun ([JValue] -> JValue)
            | JUndefined

instance Show JValue where
  show (JDouble d) = show d
  show (JString s) = show s
  show (JUndefined) = "undefined"
  show (JObject _ ss) = show ss
  show (JFun _) = "function"

type VarEnv = Map String JValue
type ObjEnv = Map Int (Int, [(String,JValue)]) -- fixme, still not sure how I want to handle trees of objects

type JState = State (VarEnv,ObjEnv)

lift_unary :: (JValue -> JValue) -> [JValue] -> JValue
lift_unary f = \[x] -> f x

jTypeOf :: JValue -> JValue
jTypeOf (JFun _) = JString "function"
jTypeOf (JDouble d) = JString "number"
jTypeOf (JObject _ _) = JString "object"
jTypeOf JUndefined = JString "undefined"
jTypeOf (JString _) = JString "string"

jToNum :: JValue -> JValue
jToNum (JDouble d) = (JDouble d)
jToNum (JString s) = JDouble $ read s -- could fail and crash program!!
                     -- deviate from spec slightly, say undefined for these
jToNum (JObject _ _) = JUndefined
jToNum JUndefined = JUndefined 
jToNum (JFun _) = JUndefined

evalExp :: Exp -> JState JValue
evalExp (NumLit d) = return $ JDouble d
evalExp (StrLit s) = return $ JString s
evalExp (ObjLit ss) = undefined
evalExp (Var s) = lookupVar s 

evalStmt :: Statement -> JState ()
evalStmt = return ()