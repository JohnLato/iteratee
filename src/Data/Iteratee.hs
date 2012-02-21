{- | Provide iteratee-based IO as described in Oleg Kiselyov's paper 'http://okmij.org/ftp/Haskell/Iteratee/'.

Oleg's original code uses lists to store buffers of data for reading in the iteratee.  This package allows the use of arbitrary types through use of the ListLike type class.

Iteratees can be thought of as stream processor combinators.  Iteratees are combined to run in sequence or in parallel, and then processed by enumerators.  The result of the enumeration is another iteratee, which may then be used again, or have the result obtained via the 'run' function.

> -- count the number of bytes in a file, reading at most 8192 bytes at a time
> import Data.Iteratee as I
> import Data.Iteratee.IO
> import Data.ByteString
> 
> byteCounter :: Monad m => Iteratee ByteString m Int
> byteCounter = I.length
> 
> countBytes = do
>   i' <- enumFile 8192 "/usr/share/dict/words" byteCounter
>   result <- run i'
>   print result

Iteratees can be combined to perform much more complex tasks.  The iteratee monad allows for sequencing iteratee operations.

> iter2 = do
>   I.drop 4
>   I.head

In addition to enumerations over files and Handles, enumerations can be programmatically generated.

> get5thElement = enumPure1Chunk [1..10] iter2 >>= run >>= print

Iteratees can also work as stream transformers, called 'Enumeratee's.  A very simple example is provided by 'Data.Iteratee.ListLike.filter'.  When working with enumeratees, it's very common to collaps the nested iteratee with 'joinI'.

This function returns the 5th element greater than 5.

> iterfilt = joinI $ I.filter (>5) iter2
> find5thOver5 = enumPure1Chunk [10,1,4,6,7,4,2,8,5,9::Int] iterfilt >>= run >>= print

Another common use of iteratees is 'takeUpTo', which guarantees that an iteratee consumes a bounded number of elements.  This is often useful when parsing data.  You can check how much data an iteratee has consumed with 'enumWith'

> iter3 :: (Num el, Ord el, Monad m) => Iteratee [el] m (el,Int)
> iter3 = joinI (I.takeUpTo 100 (enumWith iterfilt I.length))

Many more functions are provided, and there are many other useful ways to combine iteratees and enumerators.

-}

module Data.Iteratee (
  module Data.Iteratee.Binary,
  module Data.Iteratee.ListLike,
  module Data.Iteratee.PTerm,
  fileDriver,
  fileDriverVBuf,
  fileDriverRandom,
  fileDriverRandomVBuf
)

where

import Data.Iteratee.Binary
import Data.Iteratee.IO
import Data.Iteratee.ListLike
import Data.Iteratee.PTerm
