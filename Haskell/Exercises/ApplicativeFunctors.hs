data AT a = L a | B (AT a) (AT a) deriving (Show)

--functor instance of AT
instance Functor AT where
  fmap f (L a)   = L (f a)
  fmap f (B l r) = B (fmap f l) (fmap f r)

--applicative instance of AT
instance Applicative AT where
  pure = L
  (L f) <*> b   = fmap f b
  f <*> (L b)   = fmap ($ b) f
  B f1 f2 <*> b = B (f1 <*> b) (f2 <*> b)

--instance monad of AT
instance Monad AT where
  return = L
  (L b) >>= f   = f b
  (B l r) >>= f = B (l >>= f) (r >>= f)

somefunction :: AT String
somefunction = do L "on" >>= (\x -> let r = "so far <"++x++">" in
                                        B (L r) (L " it goes on")
                                        --B (L r) (L (" it goes on"++ (concat $ (replicate 4 x)))))
                         >>= (\y -> let f = "and finally <"++y++">" in
                                        B (L (f++"man i am done")) (L " coool")))

                                        --B (L (f++"man i am done")) (L (" coool"++ (concat $ (replicate 7 f)))))


main :: IO ()
main = do
         result <- return somefunction
         putStrLn (show result)
