import Data.List                     (intersperse)
import Data.Char                     (toUpper, ord)

data Michael a                       = First a | Second a (Michael a)

instance (Show a) => Show (Michael a) where
  show (First a)                     = " First [ "++(show a)++" ] "
  show (Second a tree)               = " Second [ "++(show a)++(show tree)++" ] "

--fmap       :: (Functor f) => (a -> b) -> fa -> fb
instance Functor Michael where
  fmap f (First a)                   = First (f a)
  fmap f (Second a tree)             = Second (f a) (fmap f tree)

-- pure      :: (Functor f) => a          -> fa
-- (<*>)     :: (Functor f) => f (a -> b) -> fa -> fb
instance Applicative Michael where
  pure                               = First
  (First f)    <*> (First a)         = pure (f a)
  (First f)    <*> (Second a tree)   = Second (f a) ((pure f) <*> tree)
  (Second f _) <*> (Second a tree)   = Second (f a) ((pure f) <*> tree)


--foldMap    :: (Monoid m) => (a -> m) -> ta -> m
instance Foldable Michael where
  foldMap f (First a)                = f a
  foldMap f (Second a tree)          = (f a) `mappend` (foldMap f tree)


--traverse   :: (Traversable t, Applicative f) => (a -> fb) -> ta -> f(tb)
--sequenceA  :: (Traversable t, Applicative f) => t(fa)     -> f(ta)
instance Traversable Michael where
  traverse g (First a)               = First  <$> (g a)
  traverse g (Second a tree)         = Second <$> (g a) <*> (traverse g tree)
  
  

-- return                 :: a  -> ma
-- monadic bind (>=)      :: ma -> (a -> mb) -> mb
instance Monad Michael where
  return a                           = First a
  (First a) >>= f                    = f a
  (Second a tree) >>= f              = (f a) >>= (\x -> Second x (tree >>= f))

main :: IO ()
main =                    let nyika0 = First "It has been a long day, hasn't it?"
                              nyika1  = Second "Whatever bro" (Second "What's going on" (First "That's a first!"))
                              nyika2 = Second [23,4,6,3,57,45,8,2,47,9] (Second [-1,-3,-9,-12,-2,-1,2,5,3,9,7,2,3,6] (First [5,1,17,8,2,-6,-8,-2]))
                          in  do
                             --Lets experiment with our Functor
                                putStrLn " "
                                putStrLn " :: FUNCTOR :: " 
                                putStrLn . show $ fmap (\cont -> "<reversed>"++(reverse cont)++"<reversed>") nyika1
                                putStrLn " "
                             --Lets experiment with our Applicative
                                putStrLn " :: APPLICATIVE :: " 
                                putStrLn . show $ pure (\y -> ((*3).(+4).(/2)) <$> y) <*> nyika2
                                putStrLn " "
                             --Lets experiment with our Foldable
                                putStrLn " :: FOLDABLE :: " 
                                putStrLn . show $ foldr (+) 0 $ foldMap (\x -> (*7) <$> x) nyika2
                                putStrLn " "
                                putStrLn . show $ foldMap (\x -> [length x]) nyika1
                                putStrLn " "
                             --Lets experiment with our Traversable
                                putStrLn " :: TRAVERSABLE :: "
                                putStrLn . show $ traverse (\z -> intersperse '-' z) nyika0
                                putStrLn " "
                                --putStrLn . show $ traverse (\x -> (+1) <$> x) nyika2  -- output is too long
                             --Lets experiment with our Monad
                                putStrLn " :: MONAD :: "
                                putStrLn . show $ nyika1 >>= (\a -> return $ map toUpper a) >>= (\b -> return $ map ord b)
                                putStrLn " "
                                putStrLn . show $ nyika2 >>= (\s -> return (map (^2) s))
                                putStrLn " "