
data Michael a = First a | Second a (Michael a)   deriving (Show)

--fmap :: (Functor f) => (a -> b) -> fa -> fb
instance Functor Michael where
  fmap f (First a)        = First (f a)
  fmap f (Second a name)  = Second (f a) (fmap f name)

-- pure  :: (Applicative f) => a -> fa
-- (<*>) :: (Applicative f) => f (a -> b) -> fa -> fb
instance Applicative Michael where
  pure                                = First
  (First f) <*> (First a)             = First (f a)
  (Second f name) <*> (Second a tree) = Second (f a) ((pure f) <*> tree)

-- return :: a -> ma
-- (>=)   :: ma -> (a -> mb) -> mb
instance Monad Michael where
  return a              = First a
  (First a) >>= f       = f a
  (Second a tree) >>= f = (f a) >>= (\x -> Second x (tree >>= f))


main :: IO ()
main = return ()
