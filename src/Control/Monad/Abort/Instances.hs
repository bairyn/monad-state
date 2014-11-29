{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Abort.Instances
    ( mapAbortT
    ) where

import Control.Monad.Trans.Abort

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.RWS

mapAbortT :: (m (Either r a) -> n (Either r' b)) -> AbortT r m a -> AbortT r' n b
mapAbortT f = AbortT . f . unwrapAbortT

instance (MonadCont m) => MonadCont (AbortT r m) where
    callCC f = AbortT $
        callCC $ \c ->
            unwrapAbortT . f $ \a -> AbortT $ c (Right a)

instance (MonadError m) => MonadError (AbortT r m) where
    type ErrorType (AbortT r m) = ErrorType m
    throwError = lift . throwError
    catchError m h = AbortT $ catchError (unwrapAbortT m) (unwrapAbortT . h)

instance (MonadRWS m) => MonadRWS (AbortT r m)

instance (MonadReader m) => MonadReader (AbortT r m) where
    type EnvType (AbortT r m) = EnvType m
    ask = lift ask
    local f = mapAbortT $ local f

instance (MonadState m) => MonadState (AbortT r m) where
    type StateType (AbortT r m) = StateType m
    get = lift get
    put = lift . put

instance (MonadWriter m) => MonadWriter (AbortT r m) where
    type WriterType (AbortT r m) = WriterType m
    tell   = lift . tell
    listen = mapAbortT $ \m -> do
        (a, w) <- listen m
        return $! fmap (\r -> (r, w)) a
    pass   = mapAbortT $ \m -> pass $ do
        a <- m
        return $! case a of
            --e@(Left _)      -> (e, id)
            (Left  l)      -> (Left  l,      id)
            (Right  (r, f)) -> (Right r, f)
