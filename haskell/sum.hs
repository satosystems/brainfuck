import 
sum' :: (Foldable t, Num a) => t a -> a
sum' a = foldl (+) 0 a


main = do
  print $ fix

