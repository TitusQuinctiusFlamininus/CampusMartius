--Solving Sudoku in Haskell
type XLoc = Int
type YLoc = Int
type SValue = Int
type Found = Bool


data SudoCell = SudoCell (XLoc , YLoc , SValue, Found) deriving Show


--function to generate the board
createBoard :: Int -> [SudoCell]
createBoard y =map (\x -> SudoCell (x,0,0,False)) [1..y]

--function to give the list of SudoCells that are in the same row as the given SudoCell
--sameRowCells :: SudoCell -> [SudoCell]



--function to check if SudoCell to the LEFT of the current cell has the same svalue
--Param1: The Reference Cell. We want to check all cells to the left of this one
--Param2: the list of SudoCells that have the same y value
--checkLeftForValue :: SudoCell -> [SudoCell] -> Bool
--checkLeftForValue SudoCell (x, y, s, _) =
