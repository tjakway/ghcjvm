module Jvm.TypeInfo where

import Jvm.Types
import FastString

-- only variables can be constant
-- methods can be declared final but this has a totally different meaning
isConstant :: JvmVar -> Bool
isConstant (JvmVar _ []) = False
isConstant (JvmVar _ attrs) = Final `elem` attrs

getMethodSignature :: JvmClass -> JvmMethod -> MethodSpec
getMethodSignature = undefined
