
-- Random Example 
data Type1 a b c = Type1 { 
                           do1 :: (a -> b) -> a -> (b,c), 
                           do2 :: (a, b) -> (a, c)
                          }



makeType1 :: (Num x, Num y, Num q) => Type1 (x,y) [z] q
makeType1 = Type1 e f
            where e s m                 = ((($) s m) , 0)
                  f (m, [])             = (m, 10)
                  f ((x,y), (a:as))     = (((x+1),(y-1)), 1)


f1 :: (Int, Int) -> [Int]
f1 t@(a,b) = concat $ [(replicate a) . (+2) $ fst t] ++ [(replicate b) . (+3) $ snd t]



-- Gardening Example
type Rainfall = Float
data Color    = Red | Green deriving (Show)
type Height   = Float
type Name     = String
type Plant    = (Color, Height, Name)

data Garden r p =     Garden {
                           plant  :: r -> p -> [p]   -> [p],
                           prune  :: [p] -> (p -> p) -> [p],
                           uproot :: [p] -> [p] 
                           } 
                           


myGarden :: Garden Rainfall Plant
myGarden = Garden pl pr up 
           where pl r (c,h,n) []     = [(c,(h+((h+r)/7)),n)]
                 pl r (c,h,n) s      = ((c,(h+((h+r)/7)),n):s)
                 pr [] _             = []
                 pr ps c             = map c ps
                 up []               = []
                 up s                = reverse . drop 1 . reverse $ s 

shearer :: Plant -> Plant
shearer (c,h,n) =  (c, (h/3), n++"snipped")


-- Airline Experiment

data Booking n f d t = Book {    book     :: n -> f -> d -> (n, [t]),
                                 fChange  :: (n, [t]) -> f -> (n, [t]),
                                 nChange  :: (n, [t]) -> n -> (n, [t]),
                                 cancel   :: (n, [t]) -> (n, [t])
                        }

--- OPTICS

data Lens s t a b = Lens { view :: s -> a, 
                           update  :: (b, s) -> t
                         }

data Prism s t a b = Prism { match :: s -> Either a t, 
                             build :: b -> t 
                             }

data Affine s t a b = Affine { preview ::  s      -> Either a t, 
                               set     ::  (b, s) -> t
                             }


strangeAffine :: Affine (Maybe a, c) (Maybe b, c) a b
strangeAffine = Affine p st
                where p (Nothing, c)   = Right (Nothing, c)
                      p (Just a, c)    = Left a
                      st (b, (Nothing, c)) = (Nothing, c)
                      st (b, (Just a, c))  = (Just b, c)
                     
                      
                      
                      
                      



main :: IO ()
main = let do1Res        = do1  makeType1 f1 (4,6)
           do2Res        = do2  makeType1 ((8,2), [3,9])
           rain          = 75.1
           myplant       = (Green, 45.9, "Geranium") 
           plantedGarden = plant myGarden rain myplant [(Red,34.56,"Rose")]
           snipped       = prune myGarden plantedGarden shearer in 
            do putStrLn ("do1       : " ++ (show do1Res))
               putStrLn ("do2       : " ++ (show do2Res))
               putStrLn ("planted   : " ++ (show plantedGarden))
               putStrLn ("pruned    : " ++ (show snipped))
               putStrLn ("uprooted  : " ++ (show $ uproot myGarden snipped))
       