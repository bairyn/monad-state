module Control.Monad.Abort
    (
    -- * Monads that can immediately return a result
      MonadAbort(..)
    -- * The Abort monad
    , Abort
    , runAbort
    -- * The AbortT monad transformer
    , AbortT(..)
    , runAbortT
    -- * AbortT operations
    , mapAbortT
    ) where

import Control.Monad.Abort.Class
import Control.Monad.Abort.Instances (mapAbortT)
import Control.Monad.Trans.Abort (runAbort, runAbortT)

import Control.Monad.Instances ()
