module Jvm.CodeGen where

import Jvm.Types
import Jvm.Expressions
import Jvm.Instructions

stackSize :: JVMMethod -> Int
stackSize = undefined

numLocalVars :: JVMMethod -> Int
numLocalVars = undefined

-- | compile an abstract instruction into its concrete type
compileInstruction :: AbstractInstruction -> Instruction
compileInstruction = undefined

mkConst :: JVMVar -> JVMVar
mkConst (JVMVar a []) = JVMVar a [Final]
mkConst (JVMVar a attrs) = if Final `elem` attrs then (JVMVar a attrs) else (JVM a (Final : attrs))
