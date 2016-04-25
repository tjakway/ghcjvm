{-# LANGUAGE CPP #-}

-- The JVM instruction set
-- TODO: add support for interfaces (invokeinterface)

module Jvm.Instructions where

#include "HsVersions.h"

import Jvm.Types
import FastString
import GHC.Int (Int32)

type MethodSpec = FastString
type Args = JvmPrimitiveValue
type Index = Int32
type Count = Int32 -- ^ the JVM 7 spec often refers to size as "count", this convention is followed here

type Stack = [JvmPrimitiveType]
type LocalVariables = [JvmPrimitiveType]
type BinarySignature = ((Stack, LocalVariables), -- ^ Input
                        (Stack, LocalVariables)) -- ^ Output

-- | instructions and their parameters
-- does NOT subclass nativeGen.Instruction because the JVM is a stack
-- machine and well at all with code that expects registers
-- TODO: add caload, castore, checkcast, dup2_x1, dup2_x2
data Instruction
    -- | pseudo-ops
    = Comment FastString 
    | Label 
            FastString      -- ^ class we're printing
            Int             -- ^ line no. (only 1 label per line)
            FastString      -- ^ label name
    
    
    -- miscellaneous instructions
    -- **********************************
    | Ret VarNum -- ^ return to the address in the passed local variable
    | Iinc VarNum Int8 -- ^ increment variable by the amount
    | Instanceof ClassName -- ^ check if the reference on top of the stack is an instance of ClassName
                           -- if ClassName is null, pushes (int) 0
                           -- if true, pushes 1
                           -- Otherwise, pushes 0

    -- bit manipulation
    -- **********************************
    | Ishl Int32 Int32 -- ^ shift $first left by $second bits (only reads the low 5 bits from $second)
    | Ishr Int32 Int32 -- ^ arithmetic shift right

    -- loads and stores
    -- **********************************
    -- array loads and stores
    | Aaload VarNum Index -- ^ load reference from array
    | Aastore 
            JvmReference  -- ^ the array to store into 
            Index         -- ^ the index to store at
            JvmReference  -- ^ the reference to store in the array

    | Baload JvmReference Int8 -- ^ load a byte or boolean from an array
    | Bastore JvmReference Index Int8 -- ^ store a byte or boolean into the array
    | Daload JvmReference Index -- ^ load double from an array
    | Dastore JvmReference Index Double -- ^ store double into an array
    | Faload JvmReference Index 
    | Fastore JvmReference Index Float
    | Iaload JvmReference Index
    | Iastore JvmReference Index Int32

    -- loads and stores for various types
    | Aload VarNum  -- ^ load reference from the passed variable number
    | Aload_0       -- ^ load reference from variable n
    | Aload_1
    | Aload_2
    | Aload_3

    | Astore VarNum
    | Astore_0
    | Astore_1
    | Astore_2
    | Astore_3

    | bipush Int8 -- ^ push byte onto the stack

    | Dload VarNum
    | Dload_0
    | Dload_1
    | Dload_2
    | Dload_3

    | Dstore VarNum
    | Dstore_0
    | Dstore_1
    | Dstore_2
    | Dstore_3

    | Fload VarNum
    | Fload_0
    | Fload_1
    | Fload_2
    | Fload_3

    | Fstore VarNum
    | Fstore_0
    | Fstore_1
    | Fstore_2
    | Fstore_3

    | Iload VarNum
    | Iload_0
    | Iload_1
    | Iload_2
    | Iload_3

    | Istore VarNum
    | Lload VarNum
    | Lstore VarNum

    -- the following instructions push integer constants on the stack and take
    -- no operands
    | Iconst_m1 -- ^ push -1
    | Iconst_0  -- ^ push 0
    | Iconst_1  -- and so forth
    | Iconst_2
    | Iconst_3
    | Iconst_4
    | Iconst_5
    -- same as above with floats
    | Fconst_0
    | Fconst_1
    | Fconst_2
    -- longs (these are wide)
    | Lconst_0
    | Lconst_1
    -- doubles (also wide)
    | Dconst_0
    | Dconst_1
    -- pushes a null reference
    | Aconst_null
    

    -- comparisons
    -- **********************************
    | Dcmpg Double Double -- ^ if first > second, pushes (int) 1
                          --   if first == second, pushes (int) 0
                          --   if first < second, pushes (int) -1
    | Dcmpl Double Double -- ^ dcmpg and dcmpl are identical except for their treatment of NaN
                          --   for details see the spec, pages 396-397

    | fcmpg Float Float   -- ^ identical to above
    | fcmpl Float Float

    -- branch instructions
    -- **********************************
    | Goto Label -- ^ unconditional branch
                --reference comparisons
    | If_acmpeq -- ^ branch if references are equal
            Label  -- ^ the label is passed as an operand
            JvmReference  -- ^ the references are popped off the stack
            JvmReference 
    | If_acmpne Label JvmReference JvmReference -- ^ branch if references are not equal (same format as above)
    | Ifnonnull Label JvmReference              -- ^ branch if reference is not null
    | Ifnull    Label JvmReference              -- ^ branch if reference is null
    
                                    -- ^ branch when...
    | If_icmpeq Label Int32 Int32   -- ^ first == second
    | If_icmpne Label Int32 Int32   -- ^ first /= second
    | If_icmplt Label Int32 Int32   -- ^ first <  second
    | If_icmple Label Int32 Int32   -- ^ first <= second
    | If_icmpge Label Int32 Int32   -- ^ first >  second
    | If_icmpgt Label Int32 Int32   -- ^ first >= second

    -- | branch on comparison with 0
    | Ifeq Label Int32              -- ^ param == 0
    | Ifne Label Int32              -- ^ param /= 0
    | Iflt Label Int32              -- ^ param <  0
    | Ifle Label Int32              -- ^ param <= 0
    | Ifge Label Int32              -- ^ param >  0
    | Ifgt Label Int32              -- ^ param >= 0
    
    -- Arithmetic Instructions
    -- **********************************
    | Dadd Double Double
    | Ddiv Double Double -- ^ first / second
    | Dmul Double Double
    | Dneg Double        -- ^ negate the double on the top of the stack
    | Drem Double Double -- ^ first `mod` second ("remainder")
    | Dsub Double Double -- ^ first - second

    | Fadd Float Float
    | Fdiv Float Float
    | Fmul Float Float
    | Fneg Float
    | Frem Float Float
    | Fsub Float Float
    
    | Iadd Int32 Int32
    | Idiv Int32 Int32
    | Imul Int32 Int32
    | Ineg Int32 -- ^ negates the value on top of the stack
    | Irem Int32 Int32

    -- boolean logic
    -- **********************************
    | Iand Int32 Int32
    | Ior  Int32 Int32

    -- method calls
    -- the MethodSpec indicates which method to call
    -- arguments passed on the stack
    -- **********************************
    | Invokevirtual MethodSpec [Args]
    | Invokestate MethodSpec [Args]
    | Invokenonvirtual MethodSpec [Args]


    | Areturn JvmReference -- ^ return a reference to the method caller
    | Dreturn Double       -- ^ return a double to the method caller
    | Freturn Float
    | Ireturn Int32

    -- instructions to manipulate fields
    -- see FieldSpec and FieldDescriptor for an explanation of their
    -- meanings
    -- **********************************
    | Getfield FieldSpec FieldDescriptor
    | Getstatic FieldSpec FieldDescriptor
    | Putfield FieldSpec FieldDescriptor
    | Putstatic FieldSpec FieldDescriptor


    -- arrays
    -- **********************************
    | Arraylength JvmReference -- ^ pops an array reference and pushes its size
    | Anewarray                -- ^ creates a new array of references
                ClassName       -- ^ the array type -- passed as an operand
                Count           -- ^ the count (the array size) -- will be popped off the stack
    | Newarray Count -- ^ pass size

    -- "load constant", pushes a value on the stack
    -- Jasmin automatically handles converting ldc <=> ldc_w
    -- **********************************
    | Ldc JvmValue

    -- type conversion
    -- **********************************
    | D2f Double -- ^ Double -> float
    | D2i Double -- ^ Double -> int32
    | D2l Double -- ^ Double -> long
    | F2d Float  -- ^ Float  -> double
    | F2i Float  -- ^ Float  -> int32
    | F2l Float  -- ^ Float  -> long
    | I2c Int32  -- ^ Int32  -> char
    | I2d Int32
    | I2f Int32
    | I2l Int32
    | I2s Int32

    -- stack manipulation instructions
    -- **********************************
    | Dup        -- ^ duplicates the value on top of the stack
    | Dup_x1     -- ^ duplicate the top value and insert it 2 values down
    | Dup_x2     -- ^ duplicate the top value and insert it 3 values down
                 --   if there are only 2 values on the stack it behaves
                 --   identically to Dup_x1
    | Dup2       -- ^ duplicate top 2 values on the stack
                 --   identical to dup if there's only one value on the
                 --   stack

    -- Exceptions
    | athrow JvmReference -- ^ doesn't fit well in this format because athrow doesn't really "return"
                          -- instead it throws the reference, unwinds the
                          -- stack until it finds a caller and either
                          -- panics or gives that caller the popped reference
