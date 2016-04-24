{-# LANGUAGE CPP #-}

module Jvm.Types where

import FastString

#include "HsVersions.h"

import GHC.Int (Int8, Int16, Int32, Int64)

-- data types and access modifiers

data JvmAccessModifier
    = Public
    | Protected
    | Private
    deriving (Eq, Show)

-- XXX: implement
data JvmCodeLoc = JvmCodeLoc FastString

data JvmPrimitiveValue
    = JvmInt Int32
    | JvmByte Int8
    | JvmShort Int16
    | JvmLong Int64
    | JvmChar Int16 -- ^ the Jvm uses UTF-16, chars are 16 bits
    | JvmFloat Float
    | JvmDouble Double
    | JvmVoid
--    | JvmReturnAddress JvmCodeLoc
    -- XXX
    | JvmReference
    deriving (Eq, Show)

data JvmPrimitiveType
    = JvmIntType
    | JvmByteType
    | JvmShortType
    | JvmLongType
    | JvmCharType
    | JvmFloatType
    | JvmDoubleType
    | JvmVoidType
    -- WARNING-- special type, pointer to Jvm opcodes
    | JvmReturnAddressType
    deriving (Eq, Show)

data JvmType
    = JvmClassType JvmClass
    | JvmPrimType JvmPrimitiveType
    | JvmArray JvmClass
    deriving (Eq, Show)


-------------------------------------------------------------------------

data JvmClass = JvmClass
    { classAccess :: JvmAccessModifier
    , className :: FastString -- ^ the fully qualified name of the class
    , classFields :: [JvmField]
    , classConstructor :: Maybe JvmMethod
    , classAttributes :: [JvmAttribute]
    }
    deriving (Eq, Show)

-- |an array with an associated inner type
--newtype JvmArray = JvmArray { getClass :: JvmClass }



-- fields are variables with access modifiers
-- Java doesn't have any "global" variables
-- the closest thing is a public static field of a public class
data JvmField = JvmField
    { fieldVar :: JvmVar
    -- XXX: JvmVar also encapsulates JvmValue!
    , fieldInitialValue :: Maybe JvmValue -- ^ ought to be compiled into <init> aka the constructor
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
    , fieldAccess :: JvmAccessModifier
    }
    deriving (Eq, Show)

-- | WARNING: not all attributes are valid for all types
-- e.g. fields cannot be abstract
data JvmAttribute
    = Static
    | Final
    | Abstract
    | Sychronized
    | Volatile
    | Native
    deriving (Eq, Show)

data JvmMethod = JvmMethod
    { methodAccess :: JvmAccessModifier
    , methodAttributes :: [JvmAttribute]
    , methodReturnType :: JvmType
    , methodParameters :: [JvmType] -- ^ Invariant: cannot have more than 255 method parameters
    }
    deriving (Eq, Show)

type MethodSpec = FastString

-- expression types

-- XXX: Should JvmVar encapsulate JvmField or the other way around?
-- all variables are initialized to default values so a Maybe JvmValue is
-- not needed
data JvmVar = JvmVar JvmValue [JvmAttribute]
            deriving (Eq, Show)

--local variables are referenced by index, counting from 0 for static
--methods and 1 for non-static methods (local variable 0 is the this
--pointer in non-static methods) 
--the Jvm limits the number of local variables to 65535

type VarNum = Int16

data JvmLocalVar = JvmLocalVar VarNum JvmType [JvmAttribute]
                 deriving (Eq, Show)

-- | abstract type representing an intermediate value
-- will later be assigned either to local variables or the stack
data JvmValue = JvmValue JvmType JvmPrimitiveValue
              deriving (Eq, Show)

-- | the field spec is the classname and the fieldname
type FieldSpec = FastString
-- | the field descriptor is the java type of the field
type FieldDescriptor = FastString
