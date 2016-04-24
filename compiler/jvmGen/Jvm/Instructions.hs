{-# LANGUAGE CPP #-}

module Jvm.Instructions where

#include "HsVersions.h"

import Jvm.Types

data Instruction
    -- return to the address in the passed local variable
    = Ret VarNum

    -- loads and stores for various types
    | Aload VarNum
    | Astore VarNum
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

    -- instructions to manipulate fields
    -- see FieldSpec and FieldDescriptor for an explanation of their
    -- meanings
    | Getfield FieldSpec FieldDescriptor
    | Getstatic FieldSpec FieldDescriptor
    | Putfield FieldSpec FieldDescriptor
    | Putstatic FieldSpec FieldDescriptor

    | Newarray FastString

    -- "load constant", pushes a value on the stack
    -- Jasmin automatically handles converting ldc <=> ldc_w
    | Ldc JVMValue
