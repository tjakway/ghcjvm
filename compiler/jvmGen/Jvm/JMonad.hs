{-# LANGUAGE CPP #-}

-- | Jvm Monad
-- modeled on the LLVM Monad (LlvmM), see LlvmCodeGen.Base

module Jvm.JMonad where

#include "HsVersions.h"

import Jvm.Types
import BufWrite   ( BufHandle )
import Control.Monad (ap)
import DynFlags
import UniqSupply

-- version of bytecode output, NOT the version of the currently installed
-- JVM
type JvmVersion = Float

data JvmEnv = JvmEnv
    { envVersion :: JvmVersion       -- ^ target JVM version
    , envDynFlags :: DynFlags        -- ^ Dynamic flags
    , envOutput :: BufHandle         -- ^ Output buffer
    , envUniq :: UniqSupply          -- ^ Supply of unique values
    , envGeneratedClasses :: OrdList JvmClass -- ^ list of classes to output
    , 
    }

-- | JMonad, wraps state and IO
newtype JMonad a = JMonad { runJMonad :: JvmEnv -> IO (a, JvmEnv) }

instance Functor JMonad where
    fmap f m = JMonad $ \env -> do (x, env') <- runJMonad m env
                                   return (f x, env')

instance Applicative JMonad where
    pure x = JMonad $ \env -> return (x, env)
    (<*>) = ap

instance Monad JMonad where
    m >>= f  = JMonad $ \env -> do (x, env') <- runJMonad m env
                                   runJMonad (f x) env'

instance HasDynFlags JMonad where
    getDynFlags = JMonad $ \env -> return (envDynFlags env, env)
