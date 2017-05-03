data AT a = L a | B (AT a) (AT a)

--functor instance of AT
instance Functor (AT) where
  fmap f (L a)   = L (f a)
  fmap f (B l r) = B (fmap f l) (fmap f r)

--applicative instance of AT
instance Applicative AT where
  pure = L
  (L f) <*> b = fmap f b
  f <*> (L b) = fmap ($ b) f
  B (f1) (f2) <*> b = B (f1 <*> b) (f2 <*> b)

--instance monad of AT
instance Monad AT where
  return = L
  (L b) >>= f = f b
  (B l r) >>= f = B (l >>= f) (r >>= f) 

main :: IO ()
main = do return ()
