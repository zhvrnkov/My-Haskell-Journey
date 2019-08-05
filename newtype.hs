newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

data CoolBool = CoolBool { getBool :: Bool }

helloMe (CoolBool _) = "hello"

newtype WierdBool = WierdBool { getWBool :: Bool }

helloWierd (WierdBool _) = "hello wierd"

hello = helloMe $ CoolBool undefined
hello_wierd = helloWierd undefined

newtype Wrapper a = Wrapper { getList :: [a] } deriving (Show)
instance Functor Wrapper where
  fmap f (Wrapper val) = Wrapper (f val)
