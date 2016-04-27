module Jvm.Values where

import Ghc.Int (Int8, Int16, Int32, Int64)

data JvmValue
    = JvmInt Int32
    | JvmByte Int8
    | JvmShort Int16
    | JvmLong Int64
    | JvmChar Int16 -- ^ the Jvm uses UTF-16, chars are 16 bits
    | JvmFloat Float
    | JvmDouble Double
    | JvmVoid
    | JvmReference
    deriving (Eq, Show)
