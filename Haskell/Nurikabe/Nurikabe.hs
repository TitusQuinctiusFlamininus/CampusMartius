--Nurikabe : https://www.brainbashers.com/nurikabehelp.asp

--the kind of cell it is
data Cellkind = Island | Water deriving (Show)

--complete description of a single cell on the board
data NuriCell = NuriCell { x::Int, y::Int, kind::Cellkind}

main :: IO()
main = return ()
