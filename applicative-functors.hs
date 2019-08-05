import Control.Applicative

class (Functor f) => MyApplicative f where
  purify :: a -> f a
  (<+>) :: f (a -> b) -> f a -> f b

instance MyApplicative Maybe where
  purify = Just
  Nothing <+> _ = Nothing
  (Just f) <+> something = fmap f something

instance MyApplicative [] where
  purify x = [x]
  (<+>) funcs list = [ f x | f <- funcs, x <- list]

instance MyApplicative ((->) r) where
  purify x = (\r -> x)
  (<+>) rab ra = (\r -> rab r (ra r))

instance MyApplicative ZipList where
  purify x = ZipList [x]
  ZipList fs <+> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

mySeq :: (MyApplicative f) => [f a] -> f [a]
mySeq [] = purify []
mySeq (x:xs) = ((:) <$> x) <+> mySeq xs
