
osszeg :: Int -> Int -> Int
osszeg a b = a + b
hanyados3 a b = a `div` b

osztmar a b = mod a b

osztmar2 a b = a `mod` b

-- - egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> a,b -> x = (-b) / a
elsoF a b = (-b) / a

-- - egy szám abszulút értékét,
abszolut a
  | a < 0 = -a
  | otherwise = a

abszolut2 a = if a < 0 then -a else a

-- - egy szám előjelét,
elojel n = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"

elojel2 n
  | n < 0 = "negativ"
  | n > 0 = "pozitiv"
  | otherwise = "nulla"

-- - két argumentuma közül a maximumot,
max_ a b = if a > b then a else b

max1 a b
  | a > b = a
  | otherwise = b

-- - két argumentuma közül a minimumot,
min_ a b
  | a < b = a
  | otherwise = b
