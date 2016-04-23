module Jvm.TypeInfo where

import Jvm.Types
import FastString

-- only variables can be constant
-- methods can be declared final but this has a totally different meaning
isConstant :: JVMVar -> Bool
isConstant (JVMVar _ []) = False
isConstant (JVMVar _ attrs) = Final `elem` attrs

getMethodSignature :: JVMClass -> JVMMethod -> MethodSpec
getMethodSignature = undefined
