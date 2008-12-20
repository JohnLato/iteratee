module Data.Iteratee (
  module Data.Iteratee.IterateeM,
  file_driver_rb
)

where

import System.Posix
import Data.Word (Word8)
import Data.Iteratee.IterateeM
import Data.Iteratee.IO.RandomIO

-- |Process a file using the given IterateeGM
file_driver_rb :: IterateeGM Word8 RBIO a ->
               FilePath ->
               IO (Either (String, a) a)
file_driver_rb iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  rb <- rb_empty
  result <- runRB rb $ (enum_fd_random fd >. enum_eof) ==<< iter
  closeFd fd
  print_res result
 where
  print_res (IE_done a (Err err)) = return $ Left (err, a)
  print_res (IE_done a _) = return $ Right a
  print_res (IE_cont _) = return $ Left ("Iteratee unfinished", undefined)
                           
