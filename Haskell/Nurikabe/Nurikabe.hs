--Nurikabe : https://www.brainbashers.com/nurikabehelp.asp
import Data.Char
import Data.List


--the kind of cell it is
data Cellkind = Island | Water deriving (Eq, Show)

--complete description of a single cell on the board
data NuriCell = NuriCell { locX::Int, locY::Int, size::Int, kind::Cellkind } deriving (Eq, Show)

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
 | x == ',' = inputToDefault (y:z:xs)
 | otherwise = ((digitToInt x),(digitToInt y),(digitToInt z)) : inputToDefault xs

--
--FUNCTIONS TO DEAL WITH CONSTRUCTING ISLANDS OF THE CORRECT LENGTH
--
--Function params
-- a base island cell (send it in the first time as a list with 1 element)
-- the size of the island length
--to produce a list of cells that constitute the island
--This is for islands with length greater than 1
createIsland :: [NuriCell] -> Int -> [[NuriCell]]
createIsland _ 0 = [[]]
createIsland cell@(NuriCell{locX=x, locY=y, size=s, kind=_}:_) n =
 let top     = head cell
     p1      = NuriCell{locX=x+1, locY=y, size=s, kind=Island}
     p2      = NuriCell{locX=x-1, locY=y, size=s, kind=Island}
     p3      = NuriCell{locX=x, locY=y+1, size=s, kind=Island}
     p4      = NuriCell{locX=x, locY=y-1, size=s, kind=Island}
     intermed = filter ( \(NuriCell{locX=a, locY=b, size=_, kind=_}:_) -> (a >= 1 && a <= 9) && (b >= 1 && b <= 9))  [[p1,top], [p2,top], [p3,top], [p4,top]]
     insellist = nub (concat $ map (\comb -> intermed ++ createIsland comb (n-1)) intermed) in
  dropWhile (\q -> q == []) insellist




main :: IO ()
main = do
 putStrLn "***********************************************************************************************************"
 putStrLn "===================================WELCOME TO NURIKABE (from Brainbashers)==============================================="
 putStrLn "***********************************************************************************************************"
 putStrLn ""
 putStrLn "Enter values already solved on the board in the format: 123,456 etc...."
 putStrLn " For example: 123,456 would imply: 2nd cell in 1st column has value 3, 5th cell in the 4th column has value 6, etc "
 putStrLn "(Note: Traverse the board from lower left cell, moving left to right for the bottom row, then the next row, etc....)"
 inputValues <- getLine
 let hollowboard = createNuriBoard []
     defaultInput = inputToDefault inputValues in
     putStr (show (setDefaultIslands defaultInput hollowboard))
