i = [0..]
j = [0..]

r :: [Integer] -> [Integer]
r i = take 5 . filter (\x -> odd x) $ i

s :: [Integer] -> [Integer]
s j = take 5 . filter (\y -> even y) $ j

mixed = do
  let rset = r i
      sset = s j
  first  <- rset
  second <- sset
  return (first, second)
