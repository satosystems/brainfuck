main :: IO ()
main = do
  run (take 10 $ repeat '0') 0

run :: [Char] -> Int -> IO ()
run mem0 i0 = do
  -- >
  let i1 = succ i0
  -- <
  let i2 = pred i1
  -- +
  let (x0, y0) = splitAt i0 mem0
  let mem1 = x0 ++ (succ $ head y0):tail y0
  let i1 = i0
  print ((mem0, i0), (mem1, i1))

  -- .
  let (x2, y2) = splitAt i1 mem1
  c2 <- getChar
  let mem2 = x2 ++ c2:(tail y2)
  let i2 = i1
  print mem2

  -- ,
  putChar $ mem2 !! i2

  -- [
  f3 mem2 i2
  -- ]

 where
  f3 :: [Char] -> Int -> IO ()
  f3 mem3 i3 =
    if mem3 !! i3 /= '\0'
      then do
        putStrLn "hoge"
        -- ]
        f3 ('\0':(tail mem3)) i3
      else
        return ()

