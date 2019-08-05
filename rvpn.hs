solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words' 
  where foldingFunction (x:y:ys) "*" = (x * y):ys  
        foldingFunction (x:y:ys) "+" = (x + y):ys  
        foldingFunction (x:y:ys) "-" = (y - x):ys  
        foldingFunction xs numberString = read numberString:xs

words' :: String -> [String]
words' "" = []
words' solidString =
  let condition = (\c -> c /= ' ')
      firstWord = takeWhile' condition solidString
      rest      = dropWhile (not . condition) . dropWhile condition $ solidString
  in firstWord : words' rest

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' cond (x : xs) =
  if (cond x) == True
    then x : takeWhile' cond xs
    else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' cond (x : xs) =
  if (cond x) == True
    then dropWhile cond xs
    else x : dropWhile' (\c -> False) xs

pop :: [a] -> (a, [a])
pop (x : xs) = (x, xs)

push :: a -> [a] -> [a]
push x xs = (x : xs)