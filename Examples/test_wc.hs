import qualified Data.ByteString.Char8 as C
import qualified Data.Iteratee as I

import System

cnt :: I.Iteratee C.ByteString IO Int
cnt = I.liftI (step 0)
  where
    step acc (I.Chunk s)
      | C.null s = I.icont (step acc) Nothing
      | True     = let acc' = acc + C.count '\n' s in acc' `seq` I.icont (step acc') Nothing
    step acc str  = I.idone acc str

main = do
   [f] <- getArgs
   I.fileDriverVBuf (2^16) cnt f >>= print
