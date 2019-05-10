-- kakuro
-- major changes are how to implement constraints
-- how to implement black squares?
-- rewrite the checker

import Debug.Trace
import Data.List

{- Types -}
type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Int

values :: [Value]
values = [1..9]

empty :: Value
empty = 0

block :: Value
block = -1

isBlocked :: Value -> Bool
isBlocked v = v == block

isEmpty :: Value -> Bool
isEmpty v = v == empty

-- Helper Function
single :: [a] -> Bool
single [_] = True
single _ = False

-- Helper function
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xt) = (elem x xt) || hasDuplicates xt

-- Example Puzzle
-- (From Wikipedia: https://en.wikipedia.org/wiki/Kakuro)
-- Top and Left rows are always completely filled with blocked cells

x = block -- Blocked Cells
e = empty -- Empty Cells

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
solved = [[x,x,x,x,x,x,x,x],
          [x,9,7,x,x,8,7,9],
          [x,8,9,x,8,9,5,7],
          [x,6,8,5,9,7,x,x],
          [x,x,6,1,x,2,6,x],
          [x,x,x,4,6,1,3,2],
          [x,8,9,3,1,x,1,4],
          [x,3,1,2,x,x,2,1]]

type Coordinate = (Int, Int) -- (x, y) starting from top left
data Constraint = Across {start :: Coordinate, clen :: Int, csum :: Int } | Down {start :: Coordinate, clen :: Int, csum :: Int}
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


{- Check Validity of a Solution  -}

-- Check if a given solution is valid
valid :: [Constraint] -> Grid -> Bool
valid cs g = all (\c -> (sumsTo c g) && (noDups c g)) cs

-- Get values from grid corresponding to a constraint
getVals :: Constraint -> Matrix a -> [a]
getVals (Across (x,y) l _) g = take l pos -- Take the values in the entry
    where 
        row = g !! y -- Select row
        pos = drop (x+1) row -- Move to the position after the constraint

getVals (Down   (x,y) l _) g = take l pos -- Take the values in the entry
    where 
        col = (transpose g) !! x -- Select column
        pos = drop (y+1) col -- Move to the position after the constraint

-- Check if the sum condition of the constraint is satisfied
sumsTo :: Constraint -> Grid -> Bool
sumsTo c g = (sum (getVals c g)) == (csum c)

-- Check if the duplicate condition of the constraint is satisfied
noDups :: Constraint -> Grid -> Bool
noDups c g = not (hasDuplicates (getVals c g))

{- Brute Force -}

type Choices = [Value]

-- Takes a grid and returns a grid containing the possible choices in each entry
-- Kind of like penciling in the possible values
choices :: Grid -> Matrix Choices
choices g = map (map choice) g -- 2d map
            where
                choice v = if not (isBlocked v) then values else [v]

-- Converts our matrix of choices from above into a huge list of possible matrices
collapse :: Matrix [a] -> [Matrix a]
collapse = sequence . map sequence

-- Now filter our huge list of possible matrices down to those that are valid
solveBrute :: [Constraint] -> Grid -> [Grid]
solveBrute cs g = filter (valid cs) (collapse (choices g))

-- Takes forever, big surprise!!

{- Prune Second -}

-- Only want possible values that can sum to our constraint
perfectNSum :: [Int] -> [Int] -> Int -> Int -> [[Int]]
perfectNSum _      c 0 0 = [c]
perfectNSum []     _ _ _ = []
perfectNSum _      _ 0 _ = []
perfectNSum _      _ _ 0 = []
perfectNSum (v:vs) c s n = (perfectNSum vs (v:c) (s-v) (n-1)) ++ (perfectNSum vs c s n)

permuteListOfLists :: [[a]] -> [[a]]
permuteListOfLists []     = []
permuteListOfLists (x:xs) = (permutations x) ++ (permuteListOfLists xs)

-- Given Values, compute all possible combinations of n numbers that sum to s
sumCombos :: Int -> Int -> [Int] -> [[Int]]
sumCombos s n v = permuteListOfLists (perfectNSum v [] s n)


pruneSingles :: [Choices] -> [Choices]
pruneSingles cs = map (\x -> x `minus` ss) cs
    where ss = nub (concat (filter single cs))

minus :: Choices -> Choices -> Choices
minus a b = if single a then a else a \\ b


fix :: Eq a => (a -> a) -> a -> a
fix f x =  if x == x' then x else fix f x'
           where x' = f x


prune :: [Constraint] -> Matrix Choices -> Matrix Choices
prune []     g = g
prune (c:cs) g = prune cs (pruneBy c g)

pruneBy :: Constraint -> Matrix Choices -> Matrix Choices
pruneBy c = (singles c) . (sums c)

sums :: Constraint -> Matrix Choices -> Matrix Choices
sums c g = updateChoices c vals (\(x,y) -> x `intersect` y) g
    where vals = map (nub) (transpose (sumCombos (csum c) (clen c) (nub (concat (getVals c g))))) -- Dirty af

singles :: Constraint -> Matrix Choices -> Matrix Choices
singles c g = updateChoices c vals (\(x,y) -> y) g
    where vals = pruneSingles (getVals c g)

-- Disgusting
-- Takes a constraint and replace the values in the constraint with those specified
updateChoices :: Constraint -> [Choices] -> ((Choices, Choices) -> Choices) -> Matrix Choices -> Matrix Choices
updateChoices (Across (x,y) n s) new f g = 
    before ++
    [
        beforeVals ++
        (map f (zip vals new)) ++
        afterVals
    ] ++
    after
    where
        m = g
        before = take y m

        row = m !! y
        beforeVals = take (x+1) row
        vals = take n (drop (x+1) (row))
        afterVals = drop (x+n+1) row

        after = drop (y+1) m

updateChoices (Down   (x,y) n s) new f g =
    transpose (
    before ++
    [
        beforeVals ++
        (map f (zip vals new)) ++
        afterVals
    ] ++
    after
    )
    where
        m = transpose g
        before = take x m

        col = m !! x
        beforeVals = take (y+1) col
        vals = take n (drop (y+1) (col))
        afterVals = drop (y+n+1) col

        after = drop (x+1) m

solvePrune :: [Constraint] -> Grid -> [Grid]
solvePrune cs = filter (valid cs) . collapse . (prune cs) . choices

-- [[[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]],
--  [[-1],[9],[9,7],[-1],[-1],[9,8,7],[9,8,7],[9,7]],
--  [[-1],[8,9],[8,9],[-1],[9,8],[8,9,7,5],[9,8,5,7],[7,9]],
--  [[-1],[6,9,8],[7,9,8,6],[5],[8,9],[7,9,8,6,5],[-1],[-1]],
--  [[-1],[-1],[6],[4,5,3,2,1],[-1],[2,7,1,3,6,5],[6,3,2,1,5],[-1]],
--  [[-1],[-1],[-1],[3,4,2,1],[6,1,2,4,3],[1,2,3,6,4],[3,6,2,1,4],[4,2,1]],
--  [[-1],[9,2,8,3,7,4,6,5],[9,1,8,2,7,3,6,4],[2,5,3,4,1],[1,6,2,5,3,4],[-1],[2,3,1,4],[2,4,1]],
--  [[-1],[2,3],[1,2,3],[1,2,3],[-1],[-1],[1,2],[1,2]]]

-- Too slow

solveFixPrune :: [Constraint] -> Grid -> [Grid]      
solveFixPrune cs = filter (valid cs) . collapse . fix (prune cs) . choices

-- [[[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]],
--  [[-1],[9],[7],[-1],[-1],[9,8],[9,8,7],[9,7]],
--  [[-1],[8],[9],[-1],[8],[9,5],[9,5,7],[7,9]],
--  [[-1],[6],[8],[5],[9],[7],[-1],[-1]],
--  [[-1],[-1],[6],[4,3,2,1],[-1],[2,1,3,6,5],[6,3,2,1,5],[-1]],
--  [[-1],[-1],[-1],[3,4,2,1],[6,1,2,4,3],[1,2,3,6,4],[3,6,2,1,4],[4,2,1]],
--  [[-1],[9,2,8,3,7,4,6,5],[9,1,8,2,7,3,6,4],[2,3,4,1],[1,6,2,5,3,4],[-1],[2,3,1,4],[2,4,1]],
--  [[-1],[2,3],[1,2,3],[1,2,3],[-1],[-1],[1,2],[1,2]]]

--  2 * 2 * 3 * 3 * 2 * 3 * 4 * 6 * 4 * 8 * 8 * 3 * 5 * 5 * 5 * 4 * 5 * 5 * 4 * 2 * 3 * 2 * 2 * 3 * 2 = 28665446400000

-- STILL TOO SLOW 

-- {- Refine Third -}

-- Same process here as sudoku...
search :: [Constraint] -> Matrix Choices -> [Grid]
search cs m
    | blocked cs m  = []
    | complete m = collapse m
    | otherwise  = [g | m' <- guesses m
                      , g  <- search cs (fix (prune cs) m')]

guesses :: Matrix Choices -> [Matrix Choices]
guesses m =
  [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = break (any (not . single)) m
    (row1, cs : row2)    = break (not . single) row

complete :: Matrix Choices -> Bool
complete = all (all single)

blocked :: [Constraint] -> Matrix Choices -> Bool
blocked cs m = void m || not (safe cs m)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: [Constraint] -> Matrix Choices -> Bool
safe cs m = all (consistent . (\c -> getVals c m)) cs

noDups' :: Eq a => [a] -> Bool
noDups' [] = True
noDups' (x : xt) = not (elem x xt) && noDups' xt

consistent :: Row Choices -> Bool
consistent = noDups' . concat . filter single

solve :: [Constraint] -> Grid -> [Grid]
solve cs = filter (valid cs). search cs . fix (prune cs) . choices


hardPuzzle :: Grid
hardPuzzle = [[x,x,x,x,x,x,x,x,x,x],
              [x,e,e,x,e,e,x,x,e,e],
              [x,e,e,x,e,e,e,e,e,e],
              [x,e,e,e,e,x,e,e,e,e],
              [x,x,x,e,e,e,e,e,e,e],
              [x,e,e,x,e,e,x,e,e,x],
              [x,e,e,x,e,e,e,x,e,e],
              [x,x,e,e,x,e,e,x,e,e],
              [x,e,e,e,e,e,e,e,x,x],
              [x,e,e,e,e,x,e,e,e,e],
              [x,e,e,e,e,e,e,x,e,e],
              [x,e,e,x,x,e,e,x,e,e]]

hardConstraints :: [Constraint]
hardConstraints = [
    (Across (0, 1) 2 13),
    (Across (0, 2) 2  4),
    (Across (0, 3) 4 14),
    (Across (0, 5) 2 13),
    (Across (0, 6) 2  3),
    (Across (0, 8) 7 29),
    (Across (0, 9) 4 17),
    (Across (0,10) 6 38),
    (Across (0,11) 2  3),
    (Across (1, 7) 2  4),
    (Across (2, 4) 7 31),
    (Across (3, 1) 2  9),
    (Across (3, 2) 6 38),
    (Across (3, 5) 2 16),
    (Across (3, 6) 3 21),
    (Across (4, 7) 2  8),
    (Across (4,11) 2 13),
    (Across (5, 3) 4 12),
    (Across (5, 9) 4 29),
    (Across (6, 5) 2  4),
    (Across (7, 1) 2  9),
    (Across (7, 6) 2  8),
    (Across (7, 7) 2  3),
    (Across (7,10) 2 10),
    (Across (7,11) 2  9),

    (Down (1, 0) 3  7),
    (Down (1, 4) 2 11),
    (Down (1, 7) 4 11),
    (Down (2, 0) 3 16),
    (Down (2, 4) 7 29),
    (Down (3, 2) 2 13),
    (Down (3, 6) 4 16),
    (Down (4, 0) 6 22),
    (Down (4, 7) 3 17),
    (Down (5, 0) 2 10),
    (Down (5, 3) 5 34),
    (Down (5, 9) 2 16),
    (Down (6, 1) 3  9),
    (Down (6, 5) 6 24),
    (Down (7, 1) 4 13),
    (Down (7, 7) 2 14),
    (Down (8, 0) 7 29),
    (Down (8, 8) 3 10),
    (Down (9, 0) 4 29),
    (Down (9, 5) 2  4),
    (Down (9, 8) 3 24)]


prettyPrint :: Grid -> IO ()
prettyPrint = putStrLn . unlines . map (concat . map (\x -> if x < 0 then " X" else " " ++ show x))

solveDisplay :: [Constraint] -> Grid -> IO ()
solveDisplay cs g = (prettyPrint . head . (solve cs)) g

main :: IO ()
main = solveDisplay constraints puzzle
