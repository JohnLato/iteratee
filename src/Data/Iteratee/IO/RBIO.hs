module Data.Iteratee.IO.RBIO (
  RBIO (..),
  RBState (..),
  rb_empty,
  rb_seek_set,
  rb_seek_answered,
  rb_msb_first,
  rb_msb_first_set
)

where

import System.Posix
import Control.Monad.Trans
import Data.IORef


-- The type of the IO monad supporting seek requests and endianness
-- The seek_request is not-quite a state, more like a `communication channel'
-- set by the iteratee and answered by the enumerator. Since the
-- base monad is IO, it seems simpler to implement both endianness
-- and seek requests as IORef cells. Their names are grouped in a structure
-- RBState, which is propagated as the `environment.'
newtype RBIO a = RBIO{unRBIO:: RBState -> IO a}

instance Functor RBIO where
    fmap f m = RBIO( fmap f . unRBIO m )

instance Monad RBIO where
    return  = RBIO . const . return
    m >>= f = rbio_bind m f

rbio_bind :: RBIO a -> (a -> RBIO b) -> RBIO b
rbio_bind m f = RBIO( \env -> unRBIO m env >>= (\x -> unRBIO (f x) env) )

instance MonadIO RBIO where
    liftIO = RBIO . const

-- Generally, RBState is opaque and should not be used.
data RBState = RBState{msb_first :: IORef Bool,
		       seek_req  :: IORef (Maybe FileOffset) }

-- The programmer should use the following functions instead

rb_empty :: IO RBState
rb_empty = do
	   mref <- newIORef True
	   sref <- newIORef Nothing
	   return RBState{msb_first = mref, seek_req = sref}

-- To request seeking, the iteratee sets seek_req to (Just desired_offset)
-- When the enumerator answers the request, it sets seek_req back
-- to Nothing
rb_seek_set :: FileOffset -> RBIO ()
rb_seek_set off = RBIO action
 where action env = writeIORef (seek_req env) (Just off)

rb_seek_answered :: RBIO Bool
rb_seek_answered = RBIO action
 where action env = readIORef (seek_req env) >>= 
		    return . maybe True (const False)

rb_msb_first :: RBIO Bool
rb_msb_first = RBIO action
 where action env = readIORef (msb_first env)

rb_msb_first_set :: Bool -> RBIO ()
rb_msb_first_set flag = RBIO action
 where action env = writeIORef (msb_first env) flag

