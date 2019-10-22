import qualified Data.Set as Set
import Data.Fixed

type Prime = Integer

fac :: Integer -> Integer
fac 0 = 1
fac n = (* n) . fac $ (n - 1)

term :: Float -> Float
term num = (0.5 * num) * (num + 1)

term' :: Integer -> Integer
term' num
  | num < 1 = 0
  | num == 1 = 1
  | otherwise = num + term' (num - 1)

data Permutation = Permutation { n :: [Int], k :: Int }

count :: Permutation -> Int
count (Permutation objects k)
  | k == 0 = 0
  | k == 1 = n
  | otherwise = (* n) . count $ Permutation (tail objects) (k - 1)
  where n = length objects

--act :: Permutation -> [[Int]]
--act perm =

allCombinations :: Set.Set a -> [[a]]
allCombinations set =
  let list = Set.toList set
  in _allCombinations list

_allCombinations :: [a] -> [[a]]
_allCombinations [] = [[]]
_allCombinations objs =
  (objs >>= (\obj -> map (obj:) . _allCombinations . tail $ objs))

sieve :: (Integral a) => [a] -> [a]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes = take 10000 $ sieve [2..]

factorize :: Integer -> [Prime]
factorize num
  | num `elem` primes = [num]
  | otherwise = smallestPrime : factorize (num `div` smallestPrime)
  where smallestPrime = last . filter (\prime -> num `mod` prime == 0) $ primes

-- Q: why last or head doesn't matter and don't affect the result?
-- well since order of multiplication could be ignored this can be, but:
-- is there only one way to factorize any number?
-- we got `factorized_side = number`. The factorized side is the `n1 * n2 * ...`
-- so `n1 * n2 * ...` = number. The only way to transform `n1 * n2 * ...` into something else
-- is considering it as a multiplication of other numbers. For example replace n1 by its divisors
-- the only thing is the primes numbers could not be replaced by divisors since is divisbile only by one and itself

--floor' :: Fractional a => a -> Integer
--floor' n = toRational $ n - n `mod'` 1

data Exp a = Exp { base :: a, exp :: Integer } deriving (Show)

fac' :: Integer -> [Exp Prime]
fac' num = takeWhile expNotNull . map (mu num) $ primes
  where expNotNull = (\(Exp base exp) -> exp > 0)

multiply :: [Exp Prime] -> Integer
multiply exps = foldr getNum 1 exps
  where getNum = (\(Exp base exp) acc -> acc * (base ^ exp))

mu :: Integer -> Prime -> Exp Prime
mu n p = Exp p (_mu n p p)
  
_mu :: Integer -> Prime -> Integer -> Integer
_mu n p base
  | floored > 0 = floored + _mu n (p * base) base
  | otherwise = 0
  where
    n' = (fromIntegral n) :: Double
    p' = (fromIntegral p) :: Double
    floored = floor (n' / p')
