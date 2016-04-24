{-# LANGUAGE CPP #-}

module Jvm.Expressions where

#include "HsVersions.h"

import Jvm.Types

-- | a higher level of abstraction than raw Jvm instructions
-- these will be translated into their bytecode equivalents
data AbstractInstruction 
    = Ret JvmValue       -- ^ return to the address in the variable
    | Load JvmValue      -- ^ pushes the variable onto the stack
    | Store JvmValue     -- ^ pops a value into the local variable
    | Push JvmValue
    | Getfield JvmClass JvmField -- ^ get a field (static or non-static)
    | Put JvmClass JvmField JvmValue -- ^ put a value into the specified (static or non-static) field
    | Call JvmClass JvmMethod [JvmPrimitiveValue]
    | Ldc JvmValue
