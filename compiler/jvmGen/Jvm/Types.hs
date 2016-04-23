{-# LANGUAGE CPP #-}

module Jvm.Types where

#include "HsVersions.h"

-- data types and access modifiers

data JVMAccessModifier
    = Public
    | Protected
    | Private

data JVMCodeLoc = undefined

data JVMPrimitiveType
    = JVMInt
    | JVMByte
    | JVMShort
    | JVMLong
    | JVMChar
    | JVMFloat
    | JVMDouble
    | JVMVoid
    -- WARNING-- special type, pointer to JVM opcodes
    | JVMReturnAddress JVMCodeLoc

data JVMClass = JVMClass
    { classAccess :: JVMAccessModifier
    , className :: Text -- ^ the fully qualified name of the class
    , classFields :: [JVMField]
    , classConstructor :: Maybe JVMMethod
    , classAttributes :: [Attribute]
    }

-- |an array with an associated inner type
newtype JVMArray = JVMArray { getClass :: JVMClass }


data JVMType
    = JVMType JVMClass
    | JVMType JVMPrimitiveType
    | JVMArray JVMClass

-- fields are variables with access modifiers
-- Java doesn't have any "global" variables
-- the closest thing is a public static field of a public class
data JVMField = JVMField
    { fieldVar :: JVMVar 
    , fieldInitialValue :: Maybe JVMExpr -- ^ ought to be compiled into <init> aka the constructor
                                    -- could do a source-to-source
                                    -- transformation of moving all
                                    -- initial value code into the
                                    -- beginning of the class' constructor
                                    --
                                    -- fieldInitialValue isn't strictly
                                    -- necessary since we're generating
                                    -- bytecode and could treat these as
                                    -- normal stores that come first
                                    -- however, it is helpful to know that
                                    -- these ought to be done at the very
                                    -- top of <init>
    , fieldAccess :: JVMAccessModifier
    }

-- | WARNING: not all attributes are valid for all types
-- e.g. fields cannot be abstract
data JVMAttribute
    = Static
    | Final
    | Abstract
    | Sychronized
    | Volatile
    | Native

data JVMMethod = JVMMethod
    { methodAccess :: JVMAccessModifier
    , methodAttributes :: [JVMAttribute]
    , methodReturnType :: JVMType
    , methodParameters :: [JVMType] -- ^ Invariant: cannot have more than 255 method parameters
    }

-- expression types

-- XXX: Should JVMVar encapsulate JVMField or the other way around?
data JVMVar = JVMVar JVMType [JVMAttribute]

--local variables are referenced by index, counting from 0 for static
--methods and 1 for non-static methods (local variable 0 is the this
--pointer in non-static methods) 
--the JVM limits the number of local variables to 65535
data JVMLocalVar = JVMLocalVar Int16 JVMType [JVMAttribute]
