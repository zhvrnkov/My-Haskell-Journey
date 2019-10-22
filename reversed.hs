foldl' :: (a -> b -> a) -> a -> [b] -> a

foldl' action acc [] = acc
foldl' action acc (x:xs) = foldl' action (action acc x) xs

reversed :: [a] -> [a]
reversed = foldl' (\acc v -> v:acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs , p x]

--find_missing :: [a] -> [a] -> a
--find_missing (x:xs) ys =

