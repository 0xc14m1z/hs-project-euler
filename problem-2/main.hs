nextFib :: (Int, Int) -> Int
nextFib f = fst f + snd f

shouldTakeAnother :: Int -> Bool
shouldTakeAnother f = f < 4000000

addFib :: [Int] -> (Int, Int) -> [Int]
addFib list last =
  if shouldTakeAnother n
  then addFib (list ++ [n]) (n, fst last)
  else list
  where n = nextFib last

result = sum (filter even (addFib [] (1, 1)))
