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

-- ^ returns instructions for converting from one binary signature to
-- another
-- simple example: actual = (([Int], []), _); desired = (_, ([Float], [])); ought to return i2f
-- this function only evaluates the OUTPUT of _actual_ and the INPUT of
-- _desired_.  It ignores the first and second inner tuples of those 
-- respectively
-- this function is not all-knowing.  It may therefore fail and return
-- an error message as a Left
resolveBinarySignatures :: BinarySignature -> BinarySignature -> Either FastString [Instruction]
resolveBinarySignatures actual desired = undefined
