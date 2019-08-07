myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ initial [] = initial
myfoldr action initial (x:xs) =
  myfoldr action (action x initial) xs
