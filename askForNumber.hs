import System.Random
import Control.Monad(when)

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randomNumber, newGen) = randomR (0, 10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am I thinking of?  "
  userNumber <- getLine
  case reads userNumber :: [(Int, String)] of 
      [(num, "")] -> checkNumbers randomNumber num
      _ -> putStrLn "Invalid input"
  askForNumber newGen

checkNumbers :: Int -> Int -> IO ()
checkNumbers generatedNumber userNumber 
  | (generatedNumber == userNumber) = do putStrLn "Match"
  | otherwise = do putStrLn $ "MissMatch! It was: " ++ show generatedNumber
