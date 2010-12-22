{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeOperators, PostfixOperators, FlexibleInstances #-}

-- | This module is mostly self-descriptive

module Control.Monad.Record
    ( maybeAbort
    , maybeAbortM
    , MLens(..)
    , (:-->)(..)
    , getM
    , setM
    , modM
    , getMAbort
    , setMAbort
    , modMAbort
    , askM
    , liftState
    , liftSubState
    , liftSubMaybeState
    , (<:)
    , (=:)
    , ($:)
    , (<::)
    , (=::)
    , ($::)
    , (<<:)
    , (<=:)
    , (<$:)
    , (<<::)
    , (<->)
    , (<:<)
    , (>$<)
    , (>$>)
    , (>$$>)
    , module Data.Record.Label
    ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.Abort
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Record.Label hiding (getM, setM, modM, (=:), askM, localM)

maybeAbort :: (Monad m) => r -> Maybe a -> AbortT r m a
maybeAbort _ (Just x)  = return x
maybeAbort r (Nothing) = abort r

maybeAbortM :: (MonadTrans t, Monad m, MonadAbort (t m)) => AbortResultType (t m) -> MaybeT m a -> t m a
maybeAbortM r m = do
    x <- lift . runMaybeT $ m
    case x of
        (Just a)  -> return a
        (Nothing) -> abort r

class MLens l a where
    type MLensA l a
    toLens :: l f a -> f :-> Maybe (MLensA l a)

instance MLens (:->) (Maybe a) where
    type MLensA (:->) (Maybe a) = a
    toLens = id

instance MLens (:-->) a where
    type MLensA (:-->) a = a
    toLens = unMaybeLens

newtype f :--> a = MaybeLens {unMaybeLens :: f :-> Maybe a}

instance Category (:-->) where
    id  = MaybeLens $ lens Just (\a f -> maybe f id a)
    MaybeLens a . MaybeLens b = MaybeLens $ lens getter setter
        where getter f = getL a =<< getL b f
              setter   = modL b . fmap . setL a

getM :: (MonadState m) => (StateType m :-> a) -> m a
getM = gets . getL

setM :: (MonadState m) => (StateType m :-> a) -> a -> m ()
setM l = modify . setL l

modM :: (MonadState m) => (StateType m :-> a) -> (a -> a) -> m ()
modM l = modify . modL l

getMAbort :: (MonadState m, MLens l b) => r -> l (StateType (AbortT r m)) b -> (MLensA l b :-> a) -> AbortT r m a
getMAbort r b l = liftM (getL l) $ maybeAbort r =<< gets (getL $ toLens b)

setMAbort :: (MonadState m, MLens l b) => l (StateType m) b -> (MLensA l b :-> a) -> a -> m ()
setMAbort b l x = modify . modL (toLens b) . fmap $ setL l x

modMAbort :: (MonadState m, MLens l b) => l (StateType m) b -> (MLensA l b :-> a) -> (a -> a) -> m ()
modMAbort b l f = modify . modL (toLens b) . fmap $ modL l f

askM :: (MonadReader m) => (EnvType m :-> a) -> m a
askM = asks . getL

liftState :: (MonadState m) => (StateType m :-> s) -> StateT s m a -> m a
liftState l n = do
    (a, s) <- runStateT n =<< (l <::)
    l =:: s
    return a

liftSubState :: (Monad m, MonadTrans t, MonadState (t m)) => (StateType (t m) :-> s) -> StateT s m a -> t m a
liftSubState l m = do
    s       <- getM l
    (a, s') <- lift . runStateT m $ s
    setM l s'
    return a

liftSubMaybeState :: (Monad m, MonadTrans t, MonadState (t m), MLens l a) => l (StateType (t m)) a -> StateT (MLensA l a) m a1 -> MaybeT (t m) a1
liftSubMaybeState l m = MaybeT $ do
    sw <- getM l'
    case sw of
        (Just s) -> do
            (a, s') <- lift . runStateT m $ s
            setM l' $ Just s'
            return $ Just a
        (Nothing) -> do
            return Nothing
    where l' =  toLens l

-- | 'getL'
infixr 8 <::
(<:) :: (f :-> a) -> f -> a
(<:) = getL
-- | 'setL'
infixr 5 =::
(=:) :: (f :-> a) -> a -> f -> f
(=:) = setL
-- | 'modL'
infixr 8 $::
($:) :: (f :-> a) -> (a -> a) -> f -> f
($:) = modL

-- | 'getM'
infixr 8 <:
(<::) :: (MonadState m) => (StateType m :-> a) -> m a
(<::) = getM
-- | 'setM'
infixr 5 =:
(=::) :: (MonadState m) => (StateType m :-> a) -> a -> m ()
(=::) = setM
-- | 'modM'
infixr 8 $:
($::) :: (MonadState m) => (StateType m :-> a) -> (a -> a) -> m ()
($::) = modM

-- | 'getMAbort'
infixr 8 <<:
(<<:) :: (MonadState m, MLens l b) => r -> l (StateType (AbortT r m)) b -> (MLensA l b :-> a) -> AbortT r m a
(<<:) = getMAbort
-- | 'setMAbort'
infixr 5 <=:
(<=:) :: (MonadState m, MLens l b) => l (StateType m) b -> (MLensA l b :-> a) -> a -> m ()
(<=:) = setMAbort
-- | 'modMAbort'
infixr 8 <$:
(<$:) :: (MonadState m, MLens l b) => l (StateType m) b -> (MLensA l b :-> a) -> (a -> a) -> m ()
(<$:) = modMAbort

-- | 'getMAbort' ()
infixr 8 <<::
(<<::) :: (MonadState m, MLens l b) => l (StateType (AbortT r m)) b -> (MLensA l b :-> a) -> AbortT () m a
(<<::) = getMAbort ()

infixr 5 <->
(<->) :: (MLens l a, MLens l' a') => l (MLensA l' a') a -> l' f a' -> (f :--> MLensA l a)
a <-> b = (MaybeLens . toLens $ a) . (MaybeLens . toLens $ b)

-- | 'askM'
infixr 5 <:<
(<:<) :: (MonadReader m) => (EnvType m :-> a) -> m a
(<:<) = askM

-- | 'liftState'
infixr 4 >$<
(>$<) :: (MonadState m) => (StateType m :-> s) -> StateT s m a -> m a
(>$<) = liftState

-- These functions have more restrictive types than the functions to which their functionality is equivalent, to resolve some errors caused by too general types

-- | 'liftSubState'
infixr 5 >$>
(>$>) :: (Monad m) => (s :-> s') -> StateT s' m a -> StateT s m a
(>$>) = liftSubState

-- | 'liftSubMaybeState'
infixr 4 >$$>
(>$$>) :: (Monad m, MLens l a) => l s a -> StateT (MLensA l a) m a1 -> MaybeT (StateT s m) a1
(>$$>) = liftSubMaybeState
