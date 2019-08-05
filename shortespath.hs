data Section = Section { roadA :: Int,
                   roadB :: Int,
                   crossroad :: Int
                   } deriving (Show)
type RoadSystem = [Section]
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

road :: RoadSystem
road = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

min' :: (Ord a) => (a, a) -> a
min' (x1, x2) = min x1 x2

minp :: (Path, Path) -> Path 
minp (p1, p2) = if (sum $ map snd p1) <= (sum $ map snd p2)
                    then p1
                    else p2

minimalPath :: RoadSystem -> Path
minimalPath road = reverse . minp . foldl minimalStep ([], []) $ road

minimalStep :: (Path, Path) -> Section -> (Path, Path) 
minimalStep (pathA, pathB) (Section a b c) = 
  let prevA = sum $ map snd pathA
      prevB = sum $ map snd pathB
      forwardPathToA = prevA + a
      crossPathToA = prevB + b + c
      forwardPathToB = prevB + b
      crossPathToB = prevA + a + c
      newPathToA = if forwardPathToA <= crossPathToA
                      then (A,a):pathA  
                      else (C,c):(B,b):pathB
      newPathToB = if forwardPathToB <= crossPathToB
                      then (B,b):pathB  
                      else (C,c):(A,a):pathA 
  in (newPathToA, newPathToB)