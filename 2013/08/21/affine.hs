affine :: Double -> Double -> Double -> Double
affine a b x = a + b * x

-- definition of composition
compose :: (b -> c) -> (a -> b) -> (a -> c)
(f `compose` g) x = f $ g x

main :: IO ()
main = do
  print (affine 1 2 3)
  print $ affine 1 2 3
  print . affine 1 2 $ 3
  print `compose` affine 1 2 $ 3
