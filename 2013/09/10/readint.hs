import Data.Char -- ord

getdigit :: (Int, [Char]) -> [(Int, [Char])]
getdigit (n, []) = []
getdigit (n, x:xs)
  | m >= 0 && m <= 9 = [(10 * n + m, xs)]
  | otherwise        = []
  where m = ord x - ord '0'

getint :: [Char] -> [(Int, [Char])]
getint xs = getdigit (0, xs) >>= getdigit >>= getdigit

bnd :: Maybe a -> (a -> Maybe b) -> Maybe b
bnd Nothing f = Nothing
bnd (Just a) f = f a

getdigit2 :: (Int, [Char]) -> Maybe (Int, [Char])
getdigit2 (n, []) = Nothing
getdigit2 (n, x:xs)
  | m >= 0 && m <= 9 = Just (10 * n + m, xs)
  | otherwise        = Nothing
  where m = ord x - ord '0'

getint2 :: [Char] -> Maybe (Int, [Char])
getint2 xs = getdigit2 (0, xs) `bnd` getdigit2 `bnd` getdigit2
getint3 xs = getdigit2 (0, xs) >>= getdigit2 >>= getdigit2

main = do
  print $ getint "12345"
  print $ getint "1a345"
  print $ getint2 "12345"
  print $ getint2 "1a345"
  print $ getint3 "12345"
  print $ getint3 "1a345"
