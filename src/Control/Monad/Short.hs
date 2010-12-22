module Control.Monad.Short
    ( MonadShortable
    , MonadErrorShortable
    , short
    , unShort
    , MonadShort(..)
    ) where

import Control.Monad.Abort
import Control.Monad.Trans.Class

type MonadShortable r m a = AbortT r m a
type MonadErrorShortable m a = MonadShortable String m a

short :: (Monad m) => m a -> MonadShortable r m a
short = lift

unShort :: (Monad m) => MonadShortable r m r -> m r
unShort = runAbortT

-- | Minimal complete definition: unError
class (Monad m) => MonadShort m where
    unError       :: r -> MonadErrorShortable m a -> MonadShortable r m a

    unError_      :: MonadErrorShortable m a -> MonadShortable () m a
    unErrorShort  :: r -> MonadErrorShortable m r -> m r
    unErrorShort_ :: MonadErrorShortable m () -> m ()

    unError_        = unError ()
    unErrorShort  r = unShort . unError r
    unErrorShort_   = unShort . unError_
