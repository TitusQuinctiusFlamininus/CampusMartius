import Data.List                     (intersperse)
import Data.Char                     (toUpper, ord)
import Control.Lens.Type             (Lens)
import Control.Lens.Getter           (view)
import Control.Lens.Setter           (set, over)

--arbitrarily chosen value constructors
data Michael a                       = First a | Second a (Michael a)

--show       :: a -> String
instance (Show a) => Show (Michael a) where
  show (First a)                     = " First  [ "++(show a)++" ] "
  show (Second a tree)               = " Second [ "++(show a)++(show tree)++" ] "

--fmap       :: (Functor f) => (a -> b) -> fa -> fb
instance Functor Michael where
  fmap f (First a)                   = First  (f a)
  fmap f (Second a tree)             = Second (f a) (fmap f tree)

-- pure      :: (Functor f) => a          -> fa
-- (<*>)     :: (Functor f) => f (a -> b) -> fa -> fb
instance Applicative Michael where
  pure                                    = First
  (First f)        <*> (First a)          = pure (f a)
  (First f)        <*> (Second a tree)    = Second (f a) ((pure f) <*> tree)
  (Second f tree)  <*> (First a)          = pure (f a)
  (Second f tree1) <*> (Second a tree2)   = Second (f a) ((pure f) <$> tree1 <*> tree2)


--foldMap    :: (Monoid m) => (a -> m) -> ta -> m
instance Foldable Michael where
  foldMap f (First a)                = (f a)
  foldMap f (Second a tree)          = (f a) `mappend` (foldMap f tree)


--traverse   :: (Traversable t, Applicative f) => (a -> fb) -> ta -> f(tb)
--sequenceA  :: (Traversable t, Applicative f) => t(fa)     -> f(ta)
instance Traversable Michael where
  traverse f (First a)               = First  <$> (f a)
  traverse f (Second a tree)         = Second <$> (f a) <*> (traverse f tree)
  

-- return                 :: a  -> ma
-- monadic bind (>=)      :: ma -> (a -> mb) -> mb
instance Monad Michael where
  return                             = First
  (First a) >>= f                    = (f a)
  (Second a tree) >>= f              = (f a) >>= (\x -> Second x (tree >>= f))

-- Lens s a = (a -> fa) -> s -> fs
_mikey :: Functor f => (a -> f a) -> (Michael a) -> f (Michael a)
_mikey f (First a)                   = (\a -> First a)       <$> (f a)
_mikey f (Second a tree)             = (\a -> Second a tree) <$> (f a) 



main :: IO ()
main =                    let nyika0 = First  "It has been a long day, hasn't it?"
                              nyika1 = Second "Whatever bro" (Second "What's going on" (First "That's a first!"))
                              nyika2 = Second [23,4,6,3,57,45,8,2,47,9] (Second [-1,-3,-9,-12,-2,-1,2,5,3,9,7,2,3,6] (First [5,1,17,8,2,-6,-8,-2]))
                              nyika3 = First [23,4,6,3,57,45,8,2,47,9]
                              nyika4 = Second "ok" (First "go")
                              func   = (\x -> map (+9) x)
                          in  do
                             --Lets experiment with our Showable
                                putStrLn " "
                                putStrLn " :: SHOW :: " 
                                putStrLn . show $ (First  [["one"], ["two"], ["three"]])
                                putStrLn " "
                             --Lets experiment with our Functor
                                putStrLn " "
                                putStrLn " :: FUNCTOR :: " 
                                putStrLn . show $ fmap (\cont -> "<reversed>"++(reverse cont)++"<reversed>") nyika1
                                putStrLn " "
                             --Lets experiment with our Applicative
                                putStrLn " :: APPLICATIVE :: " 
                                putStrLn . show $ pure (\y -> (*3).(+4).(/2) <$> y) <*> nyika2
                                putStrLn . show $ (First func) <*> nyika3
                                putStrLn . show $ (Second func (First func)) <*> nyika2
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
                                putStrLn . show $ traverse (\m -> intersperse ':' m) nyika4
                                putStrLn " "
                                --putStrLn . show $ traverse (\x -> (+1) <$> x) nyika2  -- output is too long
                             --Lets experiment with our Monad
                                putStrLn " :: MONAD :: "
                                putStrLn . show $ nyika1 >>= (\a -> return $ map toUpper a) >>= (\b -> return $ map ord b)
                                putStrLn " "
                                putStrLn . show $ nyika2 >>= (\s -> return $ map (^2) s)
                                putStrLn " "
                             --Lets experiment with our Monad
                                putStrLn " :: LENSES :: "
                                putStrLn . show $ view _mikey nyika0
                                putStrLn . show $ view _mikey nyika1
                                putStrLn . show $ set  _mikey "my new value" nyika1
                                putStrLn . show $ over _mikey (++"...yeehaa!") nyika0
                                putStrLn . show $ view traverse nyika1
                                putStrLn . show $ view traverse nyika2
                                putStrLn " "