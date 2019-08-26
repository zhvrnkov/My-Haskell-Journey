{-
Lists, is some terms, are inefficient because of lists concatenation. They repeatedly appended and when we append list A to list B we go all way down to the end of list A and connect first element of list B to A[lastElement].next pointer

For avoiding this the Difference lists were invented - a data structure that always supports efficient appending.
-}

-- common list: [1,2,3]
-- diff list  : \xs -> [1,2,3] ++ xs

-- f `append` g = \xs -> f (g xs)
-- Consider, that:
-- f is a ("dog"++)
-- g is a ("meat"++)
-- then we got:
-- \xs -> "dog" ++ ("meat" ++ xs)
-- please make notice of efficiency point in upper lambda and in lower:
-- \xs -> "dog" ++ "meat" ++ xs

import Data.Monoid

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> xs ++ [])

o = fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
