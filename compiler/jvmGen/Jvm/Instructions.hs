{-# LANGUAGE CPP #-}

-- The JVM instruction set
-- TODO: add support for interfaces (invokeinterface)

module Jvm.Instructions where

#include "HsVersions.h"

import Jvm.Types
import FastString
import GHC.Int (Int32)

-- Instruction-specific types
type Stack = [JvmPrimitiveType] -- ^ our "stack" grows LEFT, i.e. cons pushes onto the stack

-- | only records the TYPES of input/output variables, NOT their indices
-- the actual variable allocation is handled by the code generator
-- for example: JvmInt means an integer stored in a variable, such as an
-- array index
type LocalVariables = [JvmPrimitiveType] 
                                         
                                         
                                                                                 
type BinarySignature = ((Stack, LocalVariables), -- ^ Input
                        (Stack, LocalVariables)) -- ^ Output
emptySignature = (([], []), ([], []))
mkSig = (,) -- ^ passing input and output as parameters looks better


-- | instructions and their parameters
-- does NOT subclass nativeGen.Instruction because the JVM is a stack
-- machine and well at all with code that expects registers
-- (possible) TODO: add pseudo-ops to convert variables?
-- TODO: add VarNum parameters
data Instruction
 -- | pseudo-ops
 = Comment FastString 
 | Label 
        FastString      -- ^ class we're printing
        Int             -- ^ line no. (only 1 label per line)
        FastString      -- ^ label name
 
 | Aaload
 | Aastore
 | Aconst_null
 | Aload 
 | Aload_0       
 | Aload_1
 | Aload_2
 | Aload_3
 | Anewarray ClassName
 | Areturn
 | Arraylength
 | Astore 
 | Astore_0
 | Astore_1
 | Astore_2
 | Astore_3
 | Athrow
 | Baload
 | Bastore
 | Bipush Int32
 | Caload
 | Castore
 | Checkcast ClassName
 | D2f
 | D2i
 | D2l
 | Dadd
 | Daload
 | Dastore
 | Dcmpg -- ^ these instructions don't take labels--they return on the stack
 | Dcmpl
 | Dconst_0
 | Dconst_1
 | Ddiv
 | Dload 
 | Dload_0
 | Dload_1
 | Dload_2
 | Dload_3
 | Dmul
 | Dneg
 | Drem
 | Dreturn
 | Dstore 
 | Dstore_0
 | Dstore_1
 | Dstore_2
 | Dstore_3
 | Dsub
 | Dup
 | Dup_x1
 | Dup_x2
 | Dup2
 | Dup2_x1
 | Dup2_x2
 | F2d
 | F2i
 | F2l
 | Fadd
 | Faload
 | Fastore
 | Fcmpg
 | Fcmpl
 | Fconst_0
 | Fconst_1
 | Fconst_2
 | Fdiv
 | Fload 
 | Fload_0
 | Fload_1
 | Fload_2
 | Fload_3
 | Fmul
 | Fneg
 | Frem
 | Freturn
 | Fstore 
 | Fstore_0
 | Fstore_1
 | Fstore_2
 | Fstore_3
 | Fsub
 | Getfield
 | Getstatic
 | Goto Label
 | I2b
 | I2c
 | I2d
 | I2f
 | I2l
 | I2s
 | Iadd
 | Iaload
 | Iand
 | Iastore
 | Iconst_m1 -- ^ push -1
 | Iconst_0  -- ^ push 0
 | Iconst_1  -- and so forth
 | Iconst_2
 | Iconst_3
 | Iconst_4
 | Iconst_5
 | Idiv
 | If_acmpeq Label
 | If_acmpne Label
 | If_icmpeq Label
 | If_icmpne Label
 | If_icmpge Label
 | If_icmpgt Label
 | If_icmplt Label
 | If_icmple Label
 | Ifeq
 | Ifne
 | Iflt
 | Ifle
 | Ifgt
 | Ifge
 | Ifnonnull Label
 | Ifnull Label
 | Iinc
 | Iload 
 | Iload_0
 | Iload_1
 | Iload_2
 | Iload_3
 | Imul
 | Ineg
 | Instanceof ClassName
 | Invokedynamic
 | Invokeinterface
 | Invokespecial
 | Invokestatic
 | Invokevirtual
 | Ior
 | Irem
 | Ireturn
 | Ishl
 | Ishr
 | Istore 
 | Istore_0
 | Istore_1
 | Istore_2
 | Istore_3
 | Isub
 | Iushr
 | Ixor
 | Jsr Label
 | L2d
 | L2f
 | L2i
 | Ladd
 | Laload
 | Land
 | Lastore
 | Lcmp
 | Lconst_0
 | Lconst_1
 | Ldc
 | Ldc_w
 | Ldc2_w
 | Ldiv
 | Lload
 | Lload_0
 | Lload_1
 | Lload_2
 | Lload_3
 | Lmul
 | Lneg
 | Lookupswitch
 | Lor
 | Lrem
 | Lreturn
 | Lshl
 | Lshr
 | Lstore
 | Lstore_0
 | Lstore_1
 | Lstore_2
 | Lstore_3
 | Lsub
 | Lushr
 | Lxor
 | Monitorenter
 | Monitorexit
 | Multianewarray
 | New ClassName
 | Newarray
 | Nop
 | Pop
 | Pop2
 | Putfield
 | Putstatic
 | Ret 
 | Return
 | Saload
 | Sastore
 | Sipush Int8
 | Swap
 | Tableswitch
 | Wide
deriving (Show, Eq)

class HasBinarySignature a where
        getBinarySignature :: a -> BinarySignature

instance HasBinarySignature Instruction where
        getBinarySignature i = case i of Comment _ -> emptySignature
                                         Label _ _ _ -> emptySignature
                                         Aaload -> mkSig ([JvmReference, JvmInt], []) ([JvmReference], [])
                                         Aastore ->  mkSig ([JvmReference, JvmInt, JvmReference], []) ([], [])
                                         Aconst_null -> mkSig ([], []) ([JvmReference], [])
                                         Aload -> mkSig ([], [JvmReference]) ([JvmReference], [])
                                         Aload_0 -> mkSig ([], [JvmReference]) ([JvmReference], [])
                                         Aload_1 -> mkSig ([], [JvmReference]) ([JvmReference], [])
                                         Aload_2 -> mkSig ([], [JvmReference]) ([JvmReference], [])
                                         Aload_3 -> mkSig ([], [JvmReference]) ([JvmReference], [])
                                         Iadd -> (([JvmInt, JvmInt], []), ([JvmInt], []))
                                         Isub -> (([JvmInt, JvmInt], []), ([JvmInt], []))
                                         _ -> panic "Instruction not implemented!"


-- | Nothing = does not clobber variable
-- Just = Which variable it clobbers
clobbersVariable :: Instruction -> Maybe Int
clobbersVariable i = case i of Aload_0 -> Just 0
                               Aload_1 -> Just 1
                               Aload_2 -> Just 2
                               Aload_3 -> Just 3
                               -- XXX: implement
                               _ -> Nothing
