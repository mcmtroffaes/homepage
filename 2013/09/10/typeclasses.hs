class MyMonad m where
    bnd :: m a -> (a -> m b) -> m b
    ret :: a -> m a

instance MyMonad [] where
    bnd xs f = concat . map f $ xs
    ret x = [x]

main = print $ (bnd [1,2] (\x -> [-x,x]))
