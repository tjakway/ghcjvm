module Jvm.CodeGen where

import Jvm.Types
import Jvm.Expressions
import Jvm.Instructions

stackSize :: JvmMethod -> Int
stackSize = undefined

numLocalVars :: JvmMethod -> Int
numLocalVars = undefined

-- | compile an abstract instruction into its concrete type
compileInstruction :: AbstractInstruction -> Instruction
compileInstruction = undefined

mkConst :: JvmVar -> JvmVar
mkConst (JvmVar a []) = JvmVar a [Final]
mkConst (JvmVar a attrs) = if Final `elem` attrs then (JvmVar a attrs) else (JvmVar a (Final : attrs))
