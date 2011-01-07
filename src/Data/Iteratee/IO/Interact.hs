module Data.Iteratee.IO.Interact (
  ioIter
)
where

import Control.Monad.IO.Class
import Data.Iteratee

-- | Use an IO function to choose what iteratee to run.
-- -- Typically this function handles user interaction and
-- -- returns with a simple iteratee such as 'head' or 'seek'.
-- --
-- -- The IO function takes a value of type 'a' as input, and
-- -- should return 'Right a' to continue, or 'Left b'
-- -- to terminate. Upon termination, ioIter will return 'Done b'.
-- --
-- -- The second argument to 'ioIter' is used as the initial input
-- -- to the IO function, and on each successive iteration the
-- -- previously returned value is used as input. Put another way,
-- -- the value of type 'a' is used like a fold accumulator.
-- -- The value of type 'b' is typically some form of control code
-- -- that the application uses to signal the reason for termination.
ioIter :: (MonadIO m, Nullable s)
       => (a -> IO (Either b (Iteratee s m a)))
       -> a
       -> Iteratee s m b
ioIter f a = either return (>>= ioIter f) =<< liftIO (f a)
{-# INLINE ioIter #-}
