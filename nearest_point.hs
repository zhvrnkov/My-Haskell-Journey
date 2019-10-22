import Data.Fixed as DF
import Data.Sort as S

pow :: Fractional a => Integer -> a -> a
pow exp base 
  | exp > 0 = foldl (*) 1 $ map (\_ -> base) [1..exp]
  | exp < 0 = 1 / (pow (abs exp) $ base)
  | otherwise = 1

even' :: RealFrac a => a -> Bool
even' = (== 0) . (`DF.mod'` 2)
odd' = not . even'

data Point = Point Float Float deriving (Show, Eq)

points = [Point x y | x <- [1..10], y <- [1..10], even' x, odd' y]

vertex = Point 3 2

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt (xSquare + ySquare)
  where square = pow 2
        xSquare = square (x1 + x2)
        ySquare = square (y1 + y2)

nearest_point :: [Point] -> Point -> Point
nearest_point points point = fst . head $ filter (\(p, dist) -> dist == minDistance) distances
  where minDistance = minimum $ map snd distances
        distances = map t points
        t = (\p -> (p, distance p point))