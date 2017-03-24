--Nurikabe : https://www.brainbashers.com/nurikabehelp.asp
import Data.Char
import Data.List
import Data.Maybe(fromJust)
import Data.Sequence(fromList, update)
import Data.Foldable (toList)
import Control.Monad.Trans.State.Lazy(StateT, put, get, execStateT)
import Control.Monad.Trans.Writer.Lazy(WriterT, tell, runWriterT)
import Control.Monad.Trans.Reader(ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

data Cellkind = Island | Water deriving (Eq, Show) --the kind of cell it is
data NuriCell = NuriCell { locX::Int, locY::Int, size::Int, kind::Cellkind } deriving (Eq, Show) --complete description of a single cell on the board
type AllIslands = [[[NuriCell]]] --list of all island possibilities for all user inputToDefault
type Strategy   = [(Int,Int)]    -- Tuple list : First Int = the index of Island in AllIsland ; Second Int = Index of list to choose within the list of islands
type Log        = [String]
type Nurikabe a = WriterT Log (ReaderT AllIslands (StateT (Strategy,[NuriCell],Log) IO)) a  --the monad stack that we will use to solve Nurikabe

--function to generate the board
createNuriBoard :: [NuriCell] -> [NuriCell]
createNuriBoard board
 | length board == 81     = board
 | otherwise                =
     let y = if (null board) then 1 else ((length board) `div` 9)+1
         noRegionBoard = map (\x -> NuriCell {locX = x, locY=y, size=0, kind=Water}) [1..9] in
         createNuriBoard (board ++ noRegionBoard)

--function to update a board with the default nurikabe values
setDefaultIslands :: [(Int, Int, Int)] -> [NuriCell] -> [NuriCell]
setDefaultIslands [] rest = rest
setDefaultIslands def@((a,b,c):ys) (cell@(NuriCell {locX=x, locY=y, size=z, kind=g}):xs)
 | a == x && b == y   = (NuriCell {locX=x, locY=y, size=c, kind=Island}) : setDefaultIslands ys xs
 | otherwise          = cell : setDefaultIslands def xs


--function to convert string input for defaut cell values to a format we know about
inputToDefault :: String -> [(Int, Int, Int)]
inputToDefault ""  = []
inputToDefault " " = []
inputToDefault (x:y:z:xs)
 | x == ',' || y == ',' || z == ',' = inputToDefault (y:z:xs)
 | otherwise = ((digitToInt x),(digitToInt y),(digitToInt z)) : inputToDefault xs

--function that yields a list of all Nuricells that have a default value
createBaseIslandList :: [NuriCell] -> [NuriCell]
createBaseIslandList  = filter (\NuriCell{locX=_, locY=_, size=r, kind=_} -> r > 0)

--function to give a list of all cells in its vicinity that could qualify as a part of an island formed when the given cell is one of the island cells
-- A cell, the Nuriboard and how many cells the island will be composed of
findCellUniverse :: NuriCell -> [NuriCell] -> Int -> [NuriCell]
findCellUniverse _ _ 0 = []
findCellUniverse cell@NuriCell{locX=x, locY=y, size=s, kind=_} brd n =
 let maxdist = n-1 in
     [cell] ++ (filter (\NuriCell{locX=a, locY=b, size=_, kind=_} -> ((a <= (x+maxdist)) &&
                                                                     (a >= (x-maxdist))) &&
                                                                     ((b <= (y+maxdist)) &&
                                                                     (b >= (y-maxdist))) )
              $ (filter (\NuriCell{locX=_, locY=_, size=e, kind=_} -> e ==0 ) brd))

--function that gives all possible combinations of groups of N of things from list L
groupPoss :: Int -> [NuriCell] -> [[NuriCell]]
groupPoss 0 _ = [[]]
groupPoss n xs =   do
                    y:xs' <- tails xs
                    ys <- groupPoss (n-1) xs'
                    return (y:ys)

--function to give the list of all lists of possibilities cells that could be islands
gatherAllUniverses :: [NuriCell] -> [NuriCell] -> [[NuriCell]]
gatherAllUniverses [] _  =  [[]]
gatherAllUniverses (b@NuriCell{locX=_, locY=_, size=s, kind=_}:bs) brd =
  let gathered = findCellUniverse b brd s : gatherAllUniverses bs brd in
  filter (\f -> f /= []) gathered

--function to group all the cell into universes, using the baselist as data
groupAllUniverses :: [NuriCell] -> [[NuriCell]] -> [[[NuriCell]]]
groupAllUniverses [] _ = [[[]]]
groupAllUniverses  (NuriCell{locX=_, locY=_, size=s, kind=_}:bs) (x:xs) = let grouped = groupPoss s x : groupAllUniverses bs xs
       in filter (\f -> f /= [[]]) grouped

--function to remove any lists that are empty in the list of lists
cleanGroupedUniverses :: [NuriCell] -> [[[NuriCell]]] -> [[[NuriCell]]]
cleanGroupedUniverses _ ([]) = [[[]]]
cleanGroupedUniverses baselist ((b:[]):as) = [[b]] ++ cleanGroupedUniverses baselist as
cleanGroupedUniverses baselist grpUnis =
  let cleaned = nub $ map(\b -> filter (\grp -> b `elem` grp ) (concat grpUnis)) baselist
      in map (\c -> filter (\m -> (m /= []) ) c ) cleaned

findNeighours :: NuriCell -> [NuriCell] -> [NuriCell]
findNeighours cell brd =
  let fwd = (locX cell)+1
      bck = (locX cell)-1
      up = (locY cell)+1
      dwn = (locY cell)-1
  in filter (\can -> ((locX can == fwd) && (locY can == locY cell)) || ((locX can == bck) && (locY can == locY cell)) || ((locX can == locX cell) && (locY can == up)) || ((locX can == locX cell) && (locY can == dwn))
            ) brd

--function that take a supposed island set of cell and a board (with default values) and sees if that combination is a real island or not
checkIfNeighboursBelong :: [NuriCell] -> [NuriCell] -> [Bool]
checkIfNeighboursBelong [] _ = [True]
checkIfNeighboursBelong (x:[]) _ = [True]
checkIfNeighboursBelong (p:ps) brd = let neighbours = findNeighours p brd in
      any (==True) (map (\g -> g `elem` neighbours) ps) : checkIfNeighboursBelong ps brd


--Function to get all the possible wide range of bridges and narrow it down to the the list that could only be real bridges
findAllBridges :: [[[NuriCell]]] -> [NuriCell] -> [[[NuriCell]]]
findAllBridges poss brd =
  let bridges = map (\w -> filter (\x -> all (==True) (checkIfNeighboursBelong x brd)) w )  poss
  in filter (/= [[]]) bridges

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--FUNCTIONS FOR FINAL VERIFICATION OF ISLAND COMBINATIONS
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--Function to check that Islands Dont Overlap and that there are no islands adjacent to each other (even though diagonal nearness is ok)
--Arg 1 = Each list represents a DIFFERENT ISLAND, so this list is, for example, all the first lists of each [[[NuriCell]]]
--When all individual checks are True, then we will have a result of TRUE (since we are ANDing && many true results)
checkNoIslandOverlapOrAdj :: [[NuriCell]] -> [NuriCell] -> Bool
checkNoIslandOverlapOrAdj ([]:_) _ = True
checkNoIslandOverlapOrAdj (_:[]) _  = True
checkNoIslandOverlapOrAdj ((a:as):bs) brd=
  let neighs              = findNeighours a brd
      singlecellcheck     =  all (==False) $ ((map (\other -> a `elem` other) bs) ++ (map (`elem` neighs) (concat bs)))
      fullislandcheck     = singlecellcheck && checkNoIslandOverlapOrAdj (as:bs) brd
  in  fullislandcheck && checkNoIslandOverlapOrAdj bs brd

--function to find a square block of cells given a single cell
--Arg 1 = The cell in question
--Arg 2 = The entire Nuri board
doesWaterBlockExist :: NuriCell -> [NuriCell] -> Bool
doesWaterBlockExist cell@NuriCell{locX=x, locY=y, size=_, kind=k} brd =
  let posscells = [[cell, NuriCell{locX=x+1, locY=y, size=0, kind=k}, NuriCell{locX=x, locY=y-1, size=0, kind=k},NuriCell{locX=x+1, locY=y-1, size=0, kind=k}],
                   [cell, NuriCell{locX=x-1, locY=y, size=0, kind=k}, NuriCell{locX=x, locY=y-1, size=0, kind=k},NuriCell{locX=x-1, locY=y-1, size=0, kind=k}],
                   [cell, NuriCell{locX=x-1, locY=y, size=0, kind=k}, NuriCell{locX=x, locY=y+1, size=0, kind=k},NuriCell{locX=x-1, locY=y+1, size=0, kind=k}],
                   [cell, NuriCell{locX=x+1, locY=y, size=0, kind=k}, NuriCell{locX=x, locY=y+1, size=0, kind=k},NuriCell{locX=x+1, locY=y+1, size=0, kind=k}]]
      indvtruths = map (\poss -> map (\p -> p `elem` brd) poss) posscells in
      any (==True) $ map (\tlist -> all (==True) tlist) indvtruths

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--ACTUAL SOLVE
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--Function to give a board representation just before we check if the board is a nurikabe solution.
--The islands have been provided as a list and everything else should be already water
--Arg 1 = the board
--Arg 2 = the list of island cells that will be set into the board
setBoardPossibility :: [NuriCell] -> [NuriCell] -> [NuriCell]
setBoardPossibility brd []  = brd
setBoardPossibility brd (rel@NuriCell{locX=a, locY=b, size=_, kind=Island}:is) =
      let newbrd = map (\brdcell@NuriCell{locX=x, locY=y, size=_, kind=_} ->  if (a==x && b==y) then rel else brdcell  ) brd
      in setBoardPossibility newbrd is

--function to construct a list of tuples that represent how we will grab and check through the island possibilities
--(X,Y) where X is the number of island possibilities for one cell and Y is the current index of island combination we are using
constructIslandStrategy :: [[[NuriCell]]] -> [(Int, Int)]
constructIslandStrategy [] = []
constructIslandStrategy (y:ys) = ((length y), 0) : constructIslandStrategy ys

--we need to set the type of the cell combinations as islands before we put them in the board as its cells
makeAllCellsIslands :: [[NuriCell]] -> [[NuriCell]]
makeAllCellsIslands islandposs = map (\igl ->
    map (\NuriCell{locX=x, locY=y, size=s, kind=_} -> NuriCell{locX=x, locY=y, size=s, kind=Island}) igl) islandposs

--will take a strategy, and a bunch of possibilities and fetch the next list of island cell possibilities
--the output of this function will be fed to the "setBoardPossibility" function to set the actual islands into the board
findNextIslandCombination :: [[[NuriCell]]] -> [(Int, Int)] -> [[NuriCell]]
findNextIslandCombination [] [] = []
findNextIslandCombination (y:ys) ((_,b):cs) = (y!!b) : findNextIslandCombination ys cs

findNextIslandStrategy :: [(Int, Int)] -> [(Int, Int)]
findNextIslandStrategy strat =
  let revlist = (reverse strat)
      workablelist = filter (\(a,b) -> (a/=1) && (b /= a-1)) revlist in
      if workablelist == [] then [(-1,-1)]
      else let workableindex = fromJust $ (head $ workablelist) `elemIndex` revlist
               rift = splitAt workableindex revlist
               newfst = map (\(e,f) -> if e/=1 then (e,0) else (e,f)) (fst rift)
               (g,h) = head $ snd rift
               newsnd = (g,h+1) : tail (snd rift) in
                reverse (newfst++newsnd)





prepNuri :: [NuriCell] -> [NuriCell] -> [[[NuriCell]]]
prepNuri baseislandlist readyboard =
  let gathereduniverses = gatherAllUniverses baseislandlist readyboard
      groupeduniverses = groupAllUniverses baseislandlist gathereduniverses
      cleaneduniverses = cleanGroupedUniverses baseislandlist groupeduniverses
      in findAllBridges cleaneduniverses readyboard --finally all possible bridges

checkNuri :: Nurikabe [NuriCell]
checkNuri = do
      trueislandlist                 <-  lift $ ask
      (strategy,readyboard,nurilog)  <-  lift $ lift $ get
      liftIO $ putStrLn  ("Will Use Strategy: "++show (strategy))
      let islandcombination          =  makeAllCellsIslands $ findNextIslandCombination trueislandlist strategy
          groundedboard              = setBoardPossibility readyboard (concat islandcombination)
          nooverlaps                 = checkNoIslandOverlapOrAdj islandcombination readyboard
          nobadwater                 = all (==False) (map (\cell -> doesWaterBlockExist cell groundedboard) groundedboard) in
          do
            liftIO $ putStrLn ("islandcombination: "++show (islandcombination))
            liftIO $ putStrLn ("nooverlaps: "++show (nooverlaps))
            liftIO $ putStrLn ("nobadwater: "++show (nobadwater))
            if (nooverlaps && nobadwater)
            then  do
                  liftIO $ putStrLn "We found a solution! Returning it...."
                  lift $ lift $ put (strategy,groundedboard,nurilog)
                  return groundedboard
            else let nexstrat = findNextIslandStrategy strategy in
              if [(-1,-1)] == nexstrat
              then do
                tell ["We found no Nurikabe solution"]
                lift $ lift $ put (strategy,groundedboard,nurilog)
                return [] --NO SOLUTION FOUND
              else do
              liftIO $ putStrLn "Heading for another round..."
              liftIO $ putStrLn ("The last Strategy that did NOT work is "++show (strategy))
              lift $ lift $ put (nexstrat,readyboard,nurilog)
              checkNuri

--just display the log of what has been going on so far when solving Nurikabe
displayTheLog :: Log -> IO()
displayTheLog thelog = sequence_ $  map (putStrLn) thelog

--just display the  of what has been going on so far when solving Nurikabe
displayTheStrategy :: Strategy -> IO()
displayTheStrategy thestrat =   putStrLn (show thestrat)

main :: IO ()
main = do
 putStrLn "***********************************************************************************************************"
 putStrLn "===================================WELCOME TO NURIKABE (from Brainbashers)==============================================="
 putStrLn "***********************************************************************************************************"
 putStrLn ""
 putStrLn "Enter values already solved on the board in the format: 123,456 etc...."
 putStrLn " For example: 123,456 would imply: 2nd cell in 1st column is an island of length 3, 5th cell in the 4th column is an island of length 6, etc "
 putStrLn "(Note: Traverse the board from lower left cell, moving left to right for the bottom row, then the next row, etc....)"
 inputValues <- getLine
 let hollowboard = createNuriBoard []
     defaultInput = inputToDefault inputValues
     readyboard = setDefaultIslands defaultInput hollowboard
     baseislandlist = createBaseIslandList readyboard
     trueislandlist = prepNuri baseislandlist readyboard
     strategy = constructIslandStrategy trueislandlist in
     do
     (finalstrat,finalNurikabeSolution,nurilog) <- execStateT (runReaderT (runWriterT checkNuri) trueislandlist) (strategy,readyboard,[])
     displayTheLog nurilog
     displayTheStrategy finalstrat
     if finalNurikabeSolution == [] then putStrLn "There was No Nurikabe Solution Found! (Recheck your island...)"
     else do
         putStrLn "!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§"
         putStrLn "!!!!!!!!!!!!!!!NURIKABE!!!!!!!!!!!!"
         putStrLn "!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§"
         putStrLn (show (finalNurikabeSolution)++(show (length finalNurikabeSolution)))
