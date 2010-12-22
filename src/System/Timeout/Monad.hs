module System.Timeout.Monad
    ( timeoutR
    , timeoutM
    , timeoutVoid
    ) where

import Control.Monad.IO.Class
import Control.Monad.Runnable
import System.Timeout

-- | Run an action that is runnable in the IO monad within a time limit
--
-- The time limit is specified in microseconds, and is subject to the same sas 'System.Timeout.timeout'.  
-- Finally, the action to be run itself is passed.  The result is an IO action, which is
-- normally lifted with 'liftIO' to be run in the same IO (see 'timeoutM').  The action,
-- when run, returns the resulting monadic value if it was run in time.  Conceptually,
-- one can think of this as running a local monad, perhaps a copy, in the IO monad;
-- the effects of running in the IO monad are unavoidably irreversible, but the resulting monadic
-- value can be optionally ignored, in which case it is not evaluated in the parent monad
-- and no effects are present in the outer layers of the monad.
timeoutR :: (Integral i, Monad m, MonadRunnable m) => i -> m a -> RunData m -> IO (Maybe (m a))
timeoutR i m d = do
    c <- timeout (fromIntegral i) . rcToIO $ run m d
    case c of
        (Just c') -> do
            return . Just $ construct c'
        (Nothing) -> do
            return $ Nothing

-- | Equivalent to 'timeoutM', but the result is the same monad, making passing the data needed to run the monad unnecessary
timeoutM :: (Integral i, Monad m, MonadIO m, MonadRunnable m) => i -> m a -> m (Maybe (m a))
timeoutM i m = liftIO . timeoutR i m =<< runData

-- | If the void action runs in time, it is run and 'True' is returned; otherwise 'False' is returned when running the action in time does not succeed
--
-- For example, to run a void action within one second,
-- one might write
--
-- > succeeded <- timeoutVoid
--
-- The effects of "action" will be fully executed if succeeded
-- is 'True'; if succeeded is 'False', the action could not run
-- in time, and only global IO actions that were performed while
-- the local monad was being evaluated will be run.
timeoutVoid :: (Integral i, Monad m, MonadIO m, MonadRunnable m) => i -> m () -> m Bool
timeoutVoid i m = maybe (return False) (>> return True) =<< timeoutM i m
