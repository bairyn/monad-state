{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Abort.Class
    ( Abort
    , AbortT(..)
    , MonadAbort(..)
    ) where

import Control.Monad.Trans.Error (Error(..), ErrorT)
import Control.Monad.Trans.Abort as Abort hiding (abort)
import qualified Control.Monad.Trans.Abort as Abort
import Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.List as List
import Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.RWS.Lazy as LazyRWS
import Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Trans.State.Lazy as LazyState
import Control.Monad.Trans.State.Strict as StrictState
import Control.Monad.Trans.Writer.Lazy as LazyWriter
import Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad.Trans

import Control.Monad ()
import Data.Monoid

class (Monad m) => MonadAbort m where
    type AbortResultType m
    abort :: (AbortResultType m) -> m a

instance (Monad m) => MonadAbort (AbortT r m) where
    type AbortResultType (AbortT r m) = r
    abort = Abort.abort
instance (MonadAbort m) => MonadAbort (IdentityT m) where
    type AbortResultType (IdentityT m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m) => MonadAbort (ListT m) where
    type AbortResultType (ListT m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m) => MonadAbort (MaybeT m) where
    type AbortResultType (MaybeT m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m, Error e) => MonadAbort (ErrorT e m) where
    type AbortResultType (ErrorT e m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m) => MonadAbort (ReaderT r m) where
    type AbortResultType (ReaderT r m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m, Monoid w) => MonadAbort (LazyRWS.RWST r w s m) where
    type AbortResultType (LazyRWS.RWST r w s m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m, Monoid w) => MonadAbort (StrictRWS.RWST r w s m) where
    type AbortResultType (StrictRWS.RWST r w s m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m) => MonadAbort (LazyState.StateT s m) where
    type AbortResultType (LazyState.StateT s m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m) => MonadAbort (StrictState.StateT s m) where
    type AbortResultType (StrictState.StateT s m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m, Monoid w) => MonadAbort (LazyWriter.WriterT w m) where
    type AbortResultType (LazyWriter.WriterT w m) = AbortResultType m
    abort = lift . abort
instance (MonadAbort m, Monoid w) => MonadAbort (StrictWriter.WriterT w m) where
    type AbortResultType (StrictWriter.WriterT w m) = AbortResultType m
    abort = lift . abort
