{-
Pairs * and 1, ++ and [] share some common properties:
+ the function takes two parameters
+ the parameters and the returned value have the same type
+ there exist such a value that doesn't change other values when used with the binary function

A mappend is associative binary function (orderd operations doesn't matter) that have "identity" value. For instance: `1` is identity value if `*` is Monoid 1 * x = z = x * 1

Also (op1 mappend op2) mappend op3 and op1 mappend (op2 mappend op3) is the same

Monoid is a type that have mappend (binary function) and mempty (identical value). Number is Monoid when * is mappend and 1 is mempty
-}

class ZV_Monoid m where -- takes concrete type in contrast with Functors and Applicative
  zv_mempty :: m
  zv_mappend :: m -> m -> m
  zv_mconcat :: [m] -> m
  zv_mconcat = foldr zv_mappend zv_mempty

{-
Laws:
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-}

instance ZV_Monoid [a] where
  zv_mempty = []
  zv_mappend = (++)

{-
numbers are Monoids in two forms: `*` and `+`
-}

newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance (Num a) => ZV_Monoid (Product a) where
  zv_mempty = Product 1
  zv_mappend (Product x) (Product y) = Product (x * y)

newtype ZV_Any = ZV_Any { getAny :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance ZV_Monoid ZV_Any where
  zv_mempty = ZV_Any False
  ZV_Any x `zv_mappend` ZV_Any y = ZV_Any (x || y)

newtype ZV_All = ZV_All { getAll :: Bool }
instance ZV_Monoid ZV_All where
  zv_mempty = ZV_All True
  ZV_All x `zv_mappend` ZV_All y = ZV_All (x && y)

instance ZV_Monoid Ordering where
  zv_mempty = EQ
  GT `zv_mappend` _ = GT
  EQ `zv_mappend` y = y
  LT `zv_mappend` _ = LT

commonLengthCompare :: String -> String -> Ordering
commonLengthCompare x y = let a = length x `compare` length y
                              b = x `compare` y
                          in if a == EQ then b else a

monoidLengthCompare :: String -> String -> Ordering
monoidLengthCompare x y = let a = length x `compare` length y
                              b = x `compare` y
                          in a `zv_mappend` b

instance (ZV_Monoid a) => ZV_Monoid (Maybe a) where
  zv_mempty = Nothing
  Nothing `zv_mappend` m = m
  m `zv_mappend` Nothing = m
  Just m1 `zv_mappend` Just m2 = Just (m1 `zv_mappend` m2)

-- Simillar for newtype Sum

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = foldMap f l `mappend`
                           f x         `mappend`
                           foldMap f r
                           
