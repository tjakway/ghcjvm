module Jvm.Invariants where

import Jvm.Types

-- | True = OK, False = Problem
type Passed = Bool

-- | the JVM only supports 255 parameters per method
methodParamLimit :: JvmMethod -> Passed
methodParamLimit (JvmMethod _ _ _ params) = (length params) <= 255
