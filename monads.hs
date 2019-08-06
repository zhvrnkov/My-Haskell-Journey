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
