module Jvm.CodeGen where

import OrdList
import Jvm.Types
import Jvm.Expressions
import Jvm.Instructions
(
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

-- | walk through each instruction and make sure the types line up
-- fail with error message if they don't
safeComputeBinarySignature :: [Instruction] -> Either FastString BinarySignature
safeComputeBinarySignature = undefined

-- | the fast and scary way
-- just pull the input from the first instruction and the output from the
-- last instruction and call it a day
-- doesn't return Either because it can never fail
unsafeComputeBinarySignature :: [Instruction] -> BinarySignature
unsafeComputeBinarySignature [] = emptySignature
unsafeComputeBinarySignature instrs = (fst . head $ instrs, snd . last $ instrs)

-- | see if the passed binary signature is correct for the passed
-- instructions
-- returns Either an error message or the same binary signature
checkBinarySignature :: [Instruction] -> BinarySignature -> Either FastString BinarySignature
checkBinarySignature = undefined

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

type CodeBlock = OrdList Instruction

-- | Modeled on the Llvm code generator's function of the same name
stmtToInstrs :: CmmNode e x -> JMonad CodeBlock
        
stmtToInstrs stmt = case stmt of

    -- | keep comments, don't keep ticks or unwind instructions
    CmmComment s         -> return (Comment s)
    CmmTick    _         -> return nilOL
    CmmUnwind  {}        -> return nilOL

