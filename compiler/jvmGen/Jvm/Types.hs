{-# LANGUAGE CPP #-}

module Jvm.Types where

import FastString

#include "HsVersions.h"

import GHC.Int (Int8, Int16, Int32, Int64)

-- data types and access modifiers

data JVMAccessModifier
    = Public
    | Protected
    | Private

--data JVMCodeLoc = undefined

data JVMPrimitiveValue
    = JVMInt Int32
    | JVMByte Int8
    | JVMShort Int16
    | JVMLong Int64
    | JVMChar Int16 -- ^ the JVM uses UTF-16, chars are 16 bits
    | JVMFloat Float
    | JVMDouble Double
    | JVMVoid
--    | JVMReturnAddress JVMCodeLoc
    -- XXX
    | JVMReference

data JVMPrimitiveType
    = JVMIntType
    | JVMByteType
    | JVMShortType
    | JVMLongType
    | JVMCharType
    | JVMFloatType
    | JVMDoubleType
    | JVMVoidType
    -- WARNING-- special type, pointer to JVM opcodes
    | JVMReturnAddressType

data JVMClass = JVMClass
    { classAccess :: JVMAccessModifier
    , className :: FastString -- ^ the fully qualified name of the class
    , classFields :: [JVMField]
    , classConstructor :: Maybe JVMMethod
    , classAttributes :: [JVMAttribute]
    }

-- |an array with an associated inner type
--newtype JVMArray = JVMArray { getClass :: JVMClass }


data JVMType
    = JVMClassType JVMClass
    | JVMPrimType JVMPrimitiveType
    | JVMArray JVMClass

-- fields are variables with access modifiers
-- Java doesn't have any "global" variables
-- the closest thing is a public static field of a public class
data JVMField = JVMField
    { fieldVar :: JVMVar
    -- XXX: JVMVar also encapsulates JVMValue!
    , fieldInitialValue :: Maybe JVMValue -- ^ ought to be compiled into <init> aka the constructor
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

type MethodSpec = FastString

-- expression types

-- XXX: Should JVMVar encapsulate JVMField or the other way around?
-- all variables are initialized to default values so a Maybe JVMValue is
-- not needed
data JVMVar = JVMVar JVMValue [JVMAttribute]

--local variables are referenced by index, counting from 0 for static
--methods and 1 for non-static methods (local variable 0 is the this
--pointer in non-static methods) 
--the JVM limits the number of local variables to 65535

type VarNum = Int16

data JVMLocalVar = JVMLocalVar VarNum JVMType [JVMAttribute]

-- | abstract type representing an intermediate value
-- will later be assigned either to local variables or the stack
data JVMValue = JVMValue JVMType JVMPrimitiveValue

-- | the field spec is the classname and the fieldname
type FieldSpec = FastString
-- | the field descriptor is the java type of the field
type FieldDescriptor = FastString
