module Values where

import Data.Map (Map)
import qualified Data.Map as M

data JValue = JDouble Double
            | JString String
            | JObject Int [(String,JValue)]
            | JFun ([JValue] -> JValue)
            | JUndefined
                     
type VarEnv = Map String JValue
type ObjEnv = () -- fixme, still not sure how I want to handle trees of objects
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

