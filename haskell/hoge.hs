import Control.Monad.ST
import Data.Char (chr, ord)
import Data.Foldable
import Data.STRef
import Data.Array.ST


main :: IO ()
main = do
  print $ sum' [1..100]
  let arr = runST $ do
      i <- newSTRef 0
      a <- newArray (0, 5) '0' :: ST s (STUArray s Int Char)
      modifySTRef i (+ 1)       -- >
      i0 <- readSTRef i         -- + (1)
      v0 <- readArray a i0      -- + (2)
      writeArray a i0 $ chr $ (ord v0) + 1  -- + (3)
      c1 <- getChar >>= ioToST  -- . (1)
      i1 <- readSTRef i         -- . (2)
      v1 <- readArray a i1      -- . (3)
      writeArray a i1 c1        -- . (4)
      getElems a
  print arr

sum' :: [Int] -> Int
sum' xs = runST $ do
  v <- newSTRef 0
  forM_ xs $ \i ->
    modifySTRef v (+ i)
  readSTRef v

