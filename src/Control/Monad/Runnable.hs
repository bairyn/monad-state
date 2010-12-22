{-# LANGUAGE TypeFamilies, FlexibleContexts, TypeSynonymInstances #-}

module Control.Monad.Runnable
    ( MonadRunnable(..)
    , RunnableContainer(..)
    ) where

import Data.Functor.Identity

-- | Monads that can be run in the IO monad
class (Monad m, RunnableContainer (RunContainer m)) => MonadRunnable m where
    type RunData       m
    type ConstructType m :: * -> *
    type RunContainer  m :: * -> *
    run       :: m a -> RunData m -> (RunContainer m) ((ConstructType m) a)
    runData   :: m (RunData m)
    construct :: ((ConstructType m) a) -> m a

class RunnableContainer m where
    rcToIO :: m a -> IO a

instance RunnableContainer IO where
    rcToIO = id

instance RunnableContainer Identity where
    rcToIO = return . runIdentity
