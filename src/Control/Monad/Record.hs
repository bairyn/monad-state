{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeOperators, PostfixOperators, FlexibleInstances #-}

-- TODO: support fclabels-1.0 with CPP; c.f. documentation for one of the
-- instance declaration of 'MLens'
module Control.Monad.Record
    ( maybeAbort
    , maybeAbortM
    , lensGS
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
    , module Data.Label
    --, module Data.Label.Abstract
    , module Data.Label.Poly
    , module Data.Label.Point
    ) where

import           Prelude                                  hiding ((.), id)
import           Control.Category
import           Control.Monad.Abort
import           Control.Monad.Reader
import           Control.Monad.State.Strict               hiding (get, modify)
import qualified Control.Monad.State.Strict as S (modify)
import           Control.Monad.Trans.Maybe
import           Data.Label
--import           Data.Label.Abstract             (Lens)
import           Data.Label.Poly                 (Lens)
import           Data.Label.Point                (Total)

maybeAbort :: (Monad m) => r -> Maybe a -> AbortT r m a
maybeAbort _ (Just x)  = return x
maybeAbort r (Nothing) = abort r

maybeAbortM :: (MonadTrans t, Monad m, MonadAbort (t m)) => AbortResultType (t m) -> MaybeT m a -> t m a
maybeAbortM r m = do
    x <- lift . runMaybeT $ m
    case x of
        (Just a)  -> return a
        (Nothing) -> abort r

-- | Create a lens out of a getter and setter.
lensGS :: (f -> a) -> (a -> f -> f) -> f :-> a
-- This function definition can be made to work with the following versions of
-- fc-labels; all ranges are inclusive:
--  * 1.0 - 1.1.7.1:
--lensGS = lens
--  * 2.0 - onward:
lensGS getter setter = lens getter (\modifier f -> setter (modifier (getter f)) f)

class MLens l a f where
    type MLensA l a f
    type MLensF l a f
    toLens :: l f a -> (MLensF l a f) :-> Maybe (MLensA l a f)
    --toLens :: l f a -> f :-> Maybe (MLensA l a f)

--instance MLens (:->) (Maybe a) (Maybe o) where
-- fc-labels versions as old as 1.0 will work with this package so long as
-- @Total@ is replaced with @(->)@, and in the import and export of
-- Data.Label.Poly, @Mono@ is replaced with @Abstract@, and finally in this
-- instance declaration, @Maybe i -> Maybe i@ is replaced with @Maybe i@ and
-- @Maybe o -> Maybe o@ is replaced with @Maybe o@; and the instructions in the
-- documentation for lensGS are applied.
--instance MLens (Lens (->)) (Maybe i) (Maybe o) where
instance MLens (Lens Total) (Maybe i -> Maybe i) (Maybe o -> Maybe o) where
    --type MLensA (Lens (->)) (Maybe i) (Maybe o) = i
    type MLensA (Lens Total) (Maybe i -> Maybe i) (Maybe o -> Maybe o) = i
    --type MLensF (Lens (->)) (Maybe i) (Maybe o) = Maybe o
    type MLensF (Lens Total) (Maybe i -> Maybe i) (Maybe o -> Maybe o) = Maybe o
    toLens = id

instance MLens (:-->) a f where
    type MLensA (:-->) a f = a
    type MLensF (:-->) a f = f
    toLens = unMaybeLens

newtype f :--> a = MaybeLens {unMaybeLens :: f :-> Maybe a}

instance Category (:-->) where
    id  = MaybeLens $ lensGS Just (\a f -> maybe f id a)
    MaybeLens a . MaybeLens b = MaybeLens $ lensGS getter setter
        where getter f = get a =<< get b f
              setter   = modify b . fmap . set a

getM :: (MonadState m) => (StateType m :-> a) -> m a
getM = gets . get

setM :: (MonadState m) => (StateType m :-> a) -> a -> m ()
setM l = S.modify . set l

modM :: (MonadState m) => (StateType m :-> a) -> (a -> a) -> m ()
modM l = S.modify . modify l

getMAbort :: (MonadState m, MLens l b f, MLensF l b f ~ StateType m) => r -> l f b -> (MLensA l b f :-> a) -> AbortT r m a
getMAbort r b l = liftM (get l) $ maybeAbort r =<< gets (get $ toLens b)

setMAbort :: (MonadState m, MLens l b f, MLensF l b f ~ StateType m) => l f b -> (MLensA l b f :-> a) -> a -> m ()
setMAbort b l x = S.modify . modify (toLens b) . fmap $ set l x

modMAbort :: (MonadState m, MLens l b f, MLensF l b f ~ StateType m) => l f b -> (MLensA l b f :-> a) -> (a -> a) -> m ()
modMAbort b l f = S.modify . modify (toLens b) . fmap $ modify l f

askM :: (MonadReader m) => (EnvType m :-> a) -> m a
askM = asks . get

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

liftSubMaybeState :: (Monad m, MonadTrans t, MonadState (t m), MLens l b f, MLensF l b f ~ StateType (t m)) => l f b -> StateT (MLensA l b f) m a -> MaybeT (t m) a
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

-- | 'get'
infixr 8 <::
(<:) :: (f :-> a) -> f -> a
(<:) = get
-- | 'set'
infixr 5 =::
(=:) :: (f :-> a) -> a -> f -> f
(=:) = set
-- | 'modify'
infixr 8 $::
($:) :: (f :-> a) -> (a -> a) -> f -> f
($:) = modify

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
(<<:) :: (MonadState m, MLens l b f, MLensF l b f ~ StateType m) => r -> l f b -> (MLensA l b f :-> a) -> AbortT r m a
(<<:) = getMAbort
-- | 'setMAbort'
infixr 5 <=:
(<=:) :: (MonadState m, MLens l b f, MLensF l b f ~ StateType m) => l f b -> (MLensA l b f :-> a) -> a -> m ()
(<=:) = setMAbort
-- | 'modMAbort'
infixr 8 <$:
(<$:) :: (MonadState m, MLens l b f, MLensF l b f ~ StateType m) => l f b -> (MLensA l b f :-> a) -> (a -> a) -> m ()
(<$:) = modMAbort

-- | 'getMAbort' ()
infixr 8 <<::
(<<::) :: (MonadState m, MLens l b f, MLensF l b f ~ StateType m) => l f b -> (MLensA l b f :-> a) -> AbortT () m a
(<<::) = getMAbort ()

infixr 5 <->
(<->) :: (MLens l a f, MLens l' a' f', MLensA l a f ~ MLensF l' a' f') => l' f' a' -> l f a -> MLensF l a f :--> MLensA l' a' f'
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
(>$$>) :: (Monad m, MonadTrans t, MonadState (t m), MLens l b f, MLensF l b f ~ StateType (t m)) => l f b -> StateT (MLensA l b f) m a -> MaybeT (t m) a
(>$$>) = liftSubMaybeState
