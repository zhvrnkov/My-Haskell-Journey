import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen  
      (secondCoin, newGen') = random newGen  
      (thirdCoin, newGen'') = random newGen'  
  in  (firstCoin, secondCoin, thirdCoin)

randoms' :: (Random a, RandomGen g) => g -> [a]
randoms' gen = let (value, newgen) = random gen in value : randoms' newgen

finiteRandoms :: (RandomGen g, Random a, Integral n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
  let (value, newGen) = random gen  
      (restOfList, finalGen) = finiteRandoms (n-1) newGen  
  in  (value:restOfList, finalGen) 