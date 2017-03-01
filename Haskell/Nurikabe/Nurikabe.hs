--Nurikabe : https://www.brainbashers.com/nurikabehelp.asp

--the kind of cell it is
data Cellkind = Island | Water deriving (Eq, Show)

--complete description of a single cell on the board
data NuriCell = NuriCell { locX::Int, locY::Int, size::Int, kind::Cellkind} deriving (Eq, Show)

--function to generate the board
createNuriBoard :: [NuriCell] -> [NuriCell]
createNuriBoard board
 | length board == 81     = board
 | otherwise                =
     let y = if (null board) then 1 else ((length board) `div` 9)+1
         noRegionBoard = map (\x -> NuriCell {locX = x, locY=y, size=0, kind=Island}) [1..9] in
         createNuriBoard (board ++ noRegionBoard)


main :: IO()
main = putStr (show (createNuriBoard []))
