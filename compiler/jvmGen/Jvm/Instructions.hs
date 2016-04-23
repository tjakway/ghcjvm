{-# LANGUAGE CPP #-}

module Jvm.Instructions where

#include "HsVersions.h"

data Instruction
    = Ret VarNum
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
