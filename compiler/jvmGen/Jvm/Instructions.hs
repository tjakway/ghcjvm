{-# LANGUAGE CPP #-}

module Jvm.Instructions where

#include "HsVersions.h"

import Jvm.Types
import FastString
import GHC.Int (Int32)

type Index = Int32
type Count = Int32

-- TODO: add caload, castore, checkcast
data Instruction
    -- return to the address in the passed local variable
    = Ret VarNum

    -- loads and stores
    -- **********************************
    -- array loads and stores
    | Aaload VarNum Index -- ^ load reference from array
    | Aastore 
            JvmReference  -- ^ the array to store into 
            Index         -- ^ the index to store at
            JvmReference  -- ^ the reference to store in the array

    | baload JvmReference Int8 -- ^ load a byte or boolean from an array
    | bastore JvmReference Index Int8 -- ^ store a byte or boolean into the array

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
    | Dstore VarNum
    | Fload VarNum
    | Fstore VarNum
    | Iload VarNum
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
    
    -- method calls
    -- the MethodSpec indicates which method to call
    | Invokevirtual MethodSpec
    | Invokestate MethodSpec
    | Invokenonvirtual MethodSpec

    | Areturn JvmReference -- ^ return a reference to the method caller

    -- instructions to manipulate fields
    -- see FieldSpec and FieldDescriptor for an explanation of their
    -- meanings
    | Getfield FieldSpec FieldDescriptor
    | Getstatic FieldSpec FieldDescriptor
    | Putfield FieldSpec FieldDescriptor
    | Putstatic FieldSpec FieldDescriptor


    -- arrays
    | Arraylength JvmReference Int32 -- ^ pops an array reference and pushes its size
    | Anewarray  -- ^ creates a new array of references
                 -- takes 3 parameters, 2 of them as operands and one of
                 -- them on the stack
            Int8  -- ^ indexbyte1 -- PASSED AS AN OPERAND
            Int8  -- ^ indexbyte2 -- PASSED AS AN OPERAND
            Count -- ^ the count -- will be popped off the stack
    | Newarray FastString

    -- "load constant", pushes a value on the stack
    -- Jasmin automatically handles converting ldc <=> ldc_w
    | Ldc JvmValue

    -- type conversion
    | D2f

    -- Exceptions
    | athrow JvmReference -- ^ doesn't fit well in this format because athrow doesn't really "return"
                          -- instead it throws the reference, unwinds the
                          -- stack until it finds a caller and either
                          -- panics or gives that caller the popped reference
