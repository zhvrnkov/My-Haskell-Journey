type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft birds (l, r)
  | abs (l + birds - r) < 4 = Just (l + birds, r)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight birds (l, r)
  | abs (r + birds - l) < 4 = Just (l, r + birds)
  | otherwise = Nothing
