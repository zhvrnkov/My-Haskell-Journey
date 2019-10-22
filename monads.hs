{-
Functor is for mapping function over some wrapped values. Something like (a -> b) (f a) = (f b)
Then we got a question: what if (a -> b) is also wrapped in something. Something like f (a -> b) and we still want to map for something with it? For that Applicative Functors come aside: (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-}

sixteen = (*) <$> (Just 2) <*> (Just 8)

{-
Monads are extension of Applicative. Main functionality is in that functions:
(>==) :: (Monad m) => m a -> (a -> m b) -> m b

> If we have a fancy value and a function that takes a normal value but returns a fancy value, how do we feed that fancy value into the function?
-}

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) f = f x

class MyMonad m where
  react :: a -> m a
  (>==) :: m a -> (a -> m b) -> m b

  (>>>) :: m a -> m b -> m b
  x >>> y = x >== \_ -> y
  
  bad :: String -> m a
  bad msg = error msg

instance MyMonad Maybe where
  react a = Just a
  Nothing >== _ = Nothing
  (Just a) >== f = (f a)
  bad _ = Nothing

foo :: Maybe String
foo =
  Just 3 >== (\x ->
  Just "!" >== (\y ->
  Just (show x ++ y)))

coolerFoo :: Maybe String
coolerFoo =
  do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

instance MyMonad [] where
  react a = [a]
  [] >== f = []
  (x:list) >== f = (f x) ++ (list >== f)
  bad _ = []

class (MyMonad m) => MyMonadPlus m where
  mymzero :: m a
  mymplus :: m a -> m a -> m a

instance MyMonadPlus [] where
  mymzero = []
  mymplus = (++)

guard :: (MyMonadPlus m) => Bool -> m ()
guard True = react ()
guard False = mymzero

filtered = [1..50] >== (\x -> guard ('7' `elem` show x) >>> return x)

{-
Interesting and tricky parts:
1. >>> operator - why it comes with MyMonad class?
2. do; x <- [1..50] - also tricky and fettered part. Code below that statement will be executed for each element. Need to understand clear relation between `<-` and `>==`
-}

-- The `do` notation is just syntactic sugar. This:
foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x + y)

-- will be translated into this:
foo :: Maybe String
foo =
  Just 3   >>= (\x ->
  Just "!" >>= (\y ->
  Just (show x + y)))

type KnightPos = (Int, Int)
type KnightMove = [KnightPos]

moveKnight :: KnightPos -> KnightMove
moveKnight (c,r) = do
  (c',r') <- [(c + 2, r + 1), (c + 2, r - 1),
              (c - 2, r + 1), (c - 2, r - 1),
              (c + 1, r + 2), (c - 1, r + 2),
              (c + 1, r - 2), (c - 1, r - 2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

in3 :: KnightPos -> KnightMove
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReactIn3 start end = end `elem` in3 start

-- Implement goto function that require start position and end position. Output is most shortest path of combinations from start to dest

goto :: KnightPos -> KnightPos -> [KnightMove]
goto start dest = gotos [[start]] dest

gotos :: [KnightMove] -> KnightPos -> [KnightMove]
gotos start dest =  
  if null filtered then gotos (moves start) dest
  else filtered
  where filtered = filter (\move -> (last move) == dest) start

moves :: [KnightMove] -> [KnightMove]
moves [] = []
moves (x:xs) = getFurtherMoves x ++ moves xs
  where getFurtherMoves = (\move ->
          map (\newPos -> move ++ [newPos]) (moveKnight . last $ move))

{-
Monad Laws
1. Left identity
`return x >>= f` <=> `f x`
2. Right identity
`m >>= return` <=> `m`
3. Associativity
`(m >>= f) >>= g` <=> `m >>= (\x -> f x >>= g)`
or
`(m <==< f) <==< g` <=> m <==< (f <==< g)`
-}

-- (<==<) :: (MyMonad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
-- f <==< g = (\x -> g x >== f)
λ> λ> 