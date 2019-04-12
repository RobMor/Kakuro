-- kakuro
-- major changes are how to implement constraints
-- how to implement black squares?
-- rewrite the checker


import Data.List

{- Types -}
type Grid = Matrix Value
type Matrix a = [Row a]
-- every row rep as string
type Row a = [a] 
type Value = Int

values :: [Value]
-- helper function
values = [1..9]

empty :: Value -> Bool
-- helper function, check if space is empty- this is currying ==
-- double equals takes 2 arg, but here == is fun x -> fun y ->
empty = (== 0)

single :: [a] -> Bool
-- helper function
single [_] = True
single _ = False

-- Helper function
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xt) = (elem x xt) || hasDuplicates xt

-- Example Puzzle
-- (From Wikipedia: https://en.wikipedia.org/wiki/Kakuro)
-- Asterisks represent cells in which numbers cannot be placed.
-- Dots are empty spaces ready to be filled with numbers.
-- Top and Left rows are always completely filled with asterisks
x = 0 -- Blocked Cells
e = 0 -- Empty Cells

puzzle :: Grid
puzzle = [[x,x,x,x,x,x,x,x],
          [x,e,e,x,x,e,e,e],
          [x,e,e,x,e,e,e,e],
          [x,e,e,e,e,e,x,x],
          [x,x,e,e,x,e,e,x],
          [x,x,x,e,e,e,e,e],
          [x,e,e,e,e,x,e,e],
          [x,e,e,e,x,x,e,e]]

solved :: Grid
solved = [[0,0,0,0,0,0,0,0],
          [0,9,7,0,0,8,7,9],
          [0,8,9,0,8,9,5,7],
          [0,6,8,5,9,7,0,0],
          [0,0,6,1,0,2,6,0],
          [0,0,0,4,6,1,3,2],
          [0,8,9,3,1,0,1,4],
          [0,3,1,2,0,0,2,1]]

type Coordinate = (Int, Int) -- (x, y) starting from top left
type Length = Int
type SumsTo = Int
data Constraint = Across {start :: Coordinate, length :: Int, sumsTo :: Int } | Down {start :: Coordinate, length :: Int, sumsTo :: Int}
    deriving Show

constraints = [
    (Across (0,1) 2 16),
    (Across (0,2) 2 17),
    (Across (0,3) 5 35),
    (Across (0,6) 4 21),
    (Across (0,7) 3  6),
    (Across (1,4) 2  7),
    (Across (2,5) 5 16),
    (Across (3,2) 4 29),
    (Across (4,1) 3 24),
    (Across (4,4) 2  8),
    (Across (5,6) 2  5),
    (Across (5,7) 2  3),
    
    (Down (1,0) 3 23),
    (Down (1,5) 2 11),
    (Down (2,0) 4 30),
    (Down (2,5) 2 10),
    (Down (3,2) 5 15),
    (Down (4,1) 2 17),
    (Down (4,4) 2  7),
    (Down (5,0) 5 27),
    (Down (6,0) 2 12),
    (Down (6,3) 4 12),
    (Down (7,0) 2 16),
    (Down (7,4) 3  7)]




-- Constraints are 
-- constraints = [
--     (0,0, Across, 2, 16),
--     (1,0, Across, 2, 17),
--     (2,0, Across, 5, 35),
--     (0,4, Across, 3, 24),
--     (1,3, Across, 4, 29),
--     (3,1, Across, 2, 7),
--     (3,4, Across, 2, 8),
--     (4,2, Across, 5, 16),
--     (5,0, Across, 4, 21),
--     (5,5, Across, 2, 5),
--     (6,0, Across, 3, 6),
--     (6,5, Across, 2, 3),
--     (0,0, Down, 3, 23),
--     (5,0, Down, 2, 11),
--     (0,1, Down, 4, 30),
--     (5,1, Down, 2, 10),
--     (2,2, Down, 5, 15),
--     (1,3, Down, 2, 17),
--     (4,3, Down, 2, 7),
--     (0,4, Down, 5, 27),
--     (0,5, Down, 2, 12),
--     (3,5, Down, 4, 12),
--     (0,6, Down, 2, 16),
--     (4,6, Down, 3, 7)
-- ]


-- Check if a given board is valid
valid :: Grid -> [Constraint] -> Bool
valid g (c:cs) = all (sums g)   cs &&
                 all (noDups g) cs

-- Get values from grid corresponding to a constraint
getVals :: Grid -> Constraint -> [Int]
getVals g (Across (x,y) l _) = take l pos
    where 
        row = g !! y
        pos = drop (x+1) row
getVals g (Down   (x,y) l _) = take l pos
    where 
        col = (transpose g) !! x
        pos = drop (y+1) col

sums :: Grid -> Constraint -> Bool
sums g c = (sum (getVals g c)) == (sumsTo c)

noDups :: Grid -> Constraint -> Bool
noDups g c = not (hasDuplicates (getVals g c))

{- Check correct -}
-- see if correct
-- enumerate all poss solutions

-- valid :: Grid -> Bool
-- valid g = all noDups (rows g) &&
--           all noDups (cols g) &&
--           all noDups (boxes g)
-- check to see if noDups is true when applied to each component


-- noDups :: Eq a => [a] -> Bool
-- noDups [] = True
-- noDups ( x : xt ) = not (elem x xt) && noDups xt

-- extract all rows, extract all col, all boxes and check all
-- rows :: Matrix a -> [Row a]
-- rows = id --identity function

-- cols :: Matrix a -> [Row a]
-- cols = transpose --built in function, turn all row into col, vice versa

-- -- alot of these functions are their own inverse

-- -- dot operator, function composition
-- -- h x = f (g x)
-- -- f . g = h

-- boxes :: Matrix a -> [Row a]
-- boxes = unpack . map cols . pack
--     where 
--         pack   = split . map split
--         split  = chop boxSize
--         unpack = map concat . concat

-- chop :: Int -> [a] -> [[a]]
-- chop n [] = []
-- chop n xs = take n xs : chop n (drop n xs)

{- Brute Force -}
-- type Choices = [Value]

-- -- matrix of choices
-- choices :: Grid -> Matrix Choices
-- -- choices is one fuzzy value- many
-- -- replace each empty cell with a list of all poss values
-- -- we want a list of concrete matricies- matrix with values in it
-- -- [Matrix Value]
-- choices g = map (map choice) g
--             where
--                 choice v = if empty v then values else [v]

-- -- this changes matrix of choices to matrix with values
-- collapse :: Matrix [a] -> [Matrix a]
-- collapse = sequence . map sequence
-- --TODO function sequence
-- -- map seq over all rows

-- -- now check if all of the enumerations are correct
-- solveBrute :: Grid -> [Grid]
-- solveBrute = filter valid . collapse . choices


-- {- Prune Second -}

-- -- need to make better ^ way too slow
-- -- find which values are already in the row
-- -- dont need to put every option in each empty box- too many options and some are not valid

-- prune :: Matrix Choices -> Matrix Choices
-- -- extract all components, apply reduce function, remap all comp
-- -- has to do with f . map reduce . f
-- prune = pruneBy boxes . pruneBy cols . pruneBy rows
--         where pruneBy f = f . map reduce . f

-- reduce :: Row Choices -> Row Choices
-- -- single cell xs - xss is just one row
-- -- get rid of all the stuff already occured in the col- any singleton cell- need to GET
-- -- subtract all the singles out of the col
-- -- 'minus' infix notation- same as minus xs singles
-- reduce xss = [ xs `minus` singles | xs <- xss]
--             where singles = concat (filter single xss)
--             -- ret [v], need to concat

-- --[x + y | x < [1, 2, 3], y <- [4, 5, 6]]
-- minus :: Choices -> Choices -> Choices
-- xs `minus` ys = if single xs then xs else xs \\ ys

-- solvePrune :: Grid -> [Grid]
-- solvePrune = filter valid . collapse . prune . choices

-- solveFixPrune :: Grid -> [Grid]
-- solveFixPrune = filter valid . collapse . fix prune . choices

-- fix :: Eq a => (a -> a) -> a -> a
-- fix f x =  if x == x' then x else fix f x'
--            where x' = f x

-- {- Refine Third -}

-- -- still too slow ^

-- solve :: Grid -> [Grid]
-- solve = search . prune . choices

-- search :: Matrix Choices -> [Grid]
-- search m
--     | blocked m   = []
--     | complete m = collapse m
--     -- some sells w nondeterministic entries
--     -- choose one sell, fix each of the options
--     -- after fixed, do more pruning
--     | otherwise  = [g | m' <- guesses m
--                        , g <- search (prune m')]

-- guesses :: Matrix Choices -> [Matrix Choices]
-- -- decompost matric into peices
-- -- get single cell out to manipuate
-- -- reconstruct the matric w the new choice
-- guesses m = 
--   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
--   where
--       -- rows 1 filled, rows atleast 1 not filled
--       (rows1, row : rows2) = break (any (not . single)) m
--       -- need correct element- do on row itself- find cell itself
--       (row1, cs : row2)    = break (not . single) row
--       -- list comp to make choice

-- complete :: Matrix Choices -> Bool
-- complete = all (all single)

-- blocked :: Matrix Choices -> Bool
-- blocked m = void m || not (safe m)

-- void :: Matrix Choices -> Bool
-- void = any (any null)

-- safe :: Matrix Choices -> Bool
-- safe cm = all consistent (rows cm) &&
--           all consistent (cols cm) &&
--           all consistent (boxes cm)

-- consistent :: Row Choices -> Bool
-- consistent = noDups . concat . filter single

-- main :: IO ()
-- main = (putStrLn . unlines . head . solve) puzzle


-- -- comp everything is a single, check
-- -- blcoked one choice empty, or u have an inconsistency
-- -- guesses make the nondeterminstic choice
