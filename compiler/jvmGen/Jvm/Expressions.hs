{-# LANGUAGE CPP #-}

module Jvm.Expressions where

#include "HsVersions.h"

import Jvm.Types

-- | a higher level of abstraction than raw JVM instructions
-- these will be translated into their bytecode equivalents
data AbstractInstruction 
    = Ret JVMValue       -- ^ return to the address in the variable
    | Load JVMValue      -- ^ pushes the variable onto the stack
    | Store JVMValue     -- ^ pops a value into the local variable
    | Push JVMValue
    | Get JVMClass JVMField -- ^ get a field (static or non-static)
    | Put JVMClass JVMField JVMValue -- ^ put a value into the specified (static or non-static) field
    
