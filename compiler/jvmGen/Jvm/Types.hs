{-# LANGUAGE CPP #-}

module Jvm where

#include "HsVersions.h"

-- data types and access modifiers

data JVMAccessModifier = undefined

data JVMCodeLoc = undefined

data JVMPrimitiveType
    = JVMInt
    | JVMByte
    | JVMShort
    | JVMLong
    | JVMChar
    | JVMFloat
    | JVMDouble
    -- WARNING-- special type, pointer to JVM opcodes
    | JVMReturnAddress JVMCodeLoc
    | JVMVoid

data JVMClass = JVMClass
    { classAccess :: JVMAccessModifier
    , className :: Text -- ^ the fully qualified name of the class
    , classFields :: [JVMField]
    , classConstructor :: Maybe JVMMethod
    , classAttributes :: [Attribute]
    }

-- |Java strings are objects but are treated specially by the JVM in many
-- ways
type JVMString = JVMClass

-- |an array with an associated inner type
newtype JVMArray = JVMArray { getClass :: JVMClass }


data JVMType
    = JVMType JVMClass
    | JVMType JVMPrimitiveType
    | JVMArray JVMClass

data JVMField = JVMField
    { fieldType :: JVMType
    , fieldInitialValue :: Maybe JVMExpr -- ^ ought to be compiled into <init> aka the constructor
                                    -- could do a source-to-source
                                    -- transformation of moving all
                                    -- initial value code into the
                                    -- beginning of the class' constructor
    , fieldAccess :: JVMAccessModifier
    , fieldAttributes :: [JVMAttribute]
    }

-- | WARNING: not all attributes are valid for all types
-- e.g. fields cannot be abstract
data JVMAttribute
    = Static
    | Final
    | Abstract
    | Sychronized
    | Volatile

data JVMMethod = JVMMethod
    { methodAccess :: JVMAccessModifier
    , methodAttributes :: [JVMAttribute]
    , methodReturnType :: JVMType
    , methodParameters :: [JVMType] -- ^ Invariant: cannot have more than 255 method parameters
    }

-- expression types

-- Java doesn't have any "global" variables
-- the closest thing is a public static field of a public class
data JVMVar 
    = JVMVar JVMField
    --local variables are referenced by index, counting from 0 for static
    --methods and 1 for non-static methods (local variable 0 is the this
    --pointer in non-static methods) 
    --the JVM limits the number of local variables to 65535
    | JVMLocalVar Int16 JVMType 
