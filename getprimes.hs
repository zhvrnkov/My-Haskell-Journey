import Data.Char
import System.Environment

sieve :: (Integral a) => [a] -> [a]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

main = do
    args <- getArgs
    let last = read (head args) :: Integer
        primes = sieve [2..last]
    putStrLn $ "number of primes: " ++ show (length primes)