funcx :: [Int]
funcx = [1,2,3]

funcy :: Int -> [Int]
funcy x = [-x, x]

funcfinal :: Int -> Int -> [Int]
funcfinal x y = [x + y ^ 3]

bind :: [Int] -> (Int -> [Int]) -> [Int]
bind zs f = concat . map f $ zs

bind2 f1 f2 x = bind (f1 x) (f2 x)

funcxy x = bind [-x,x] (\y -> [x + y ^ 3])
funcxy' x = bind (funcy x) (funcfinal x)
funcxy'' = bind2 funcy funcfinal

funcdo =
  do x <- [1,2,3]
     y <- [-x,x]
     [x + y ^ 3]  


filt :: Bool -> Int -> [Int]
filt cond x = if cond then [x] else []

fermat = [ [n,x,y,z] | n <- [3..], x <- [1..], y <- [1..],
                       z <- [1..], x ^ n + y ^ n == z ^ n]

main = do
   print $ bind funcx funcy
   print $ bind funcx $ bind2 funcy funcfinal
   print $ funcx `bind` (funcy `bind2` funcfinal)
   print $ [1,2,3] `bind` ((\x -> [-x,x]) `bind2` (\x y -> [x + y ^ 3]))
   print $ [1,2,3] `bind` (\x -> ([-x,x] `bind` (\y -> [x + y ^ 3])))
   print $ bind funcx funcxy
   print $ bind funcx funcxy'
   print $ bind funcx funcxy''
   print $ bind [1,2,3] (\x -> (bind [-x,x] (\y -> [x + y ^ 3])))
   print $ [1,2,3] >>= \x -> [-x,x] >>= \y -> [x + y ^ 3]
   print funcdo
   print $ [1,2,3] >>=
           \x -> [-x,x] >>=
           \y -> [x + y ^ 3]
   print $ [1,2,3] >>=
               (\x -> [-x,x] >>=
                   (\y -> [x + y ^ 3]))
   print $
     [1,2,3] >>=
     \x -> [-x..x] >>=
     \y -> [x + y ^ 3] >>=
     filt (odd y)
