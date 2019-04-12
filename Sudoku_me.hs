-- sudoku
-- boxes of 9
-- 1-9 every row and col and box
-- most everything we define we give a type annotation

import Data.List

{- Types -}
type Grid = Matrix Value
type Matrix a = [Row a]
-- every row rep as string
type Row a = [a] 
type Value = Char

{- Const -}
boxSize :: Int
boxSize = 3

values :: [Value]
-- helper function
values = ['1' .. '9']

empty :: Value -> Bool
-- helper function, check if space is empty- this is currying ==
-- double equals takes 2 arg, but here == is fun x -> fun y ->
empty = (== '.')

single :: [a] -> Bool
-- helper function
single [_] = True
single _ = False

{- Example -}
puzzle :: Grid
puzzle = ["2....1.38",
          "........5",
          ".7...6...",
          ".......13",
          ".981..257",
          "31....8..",
          "9..8...2.",
          ".5..69784",
          "4..25...."]

{- Check correct -}
-- see if correct
-- enumerate all poss solutions

valid :: Grid -> Bool
valid g = all noDups (rows g) &&
          all noDups (cols g) &&
          all noDups (boxes g)
-- check to see if noDups is true when applied to each component


noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups ( x : xt ) = not (elem x xt) && noDups xt

-- extract all rows, extract all col, all boxes and check all
rows :: Matrix a -> [Row a]
rows = id --identity function

cols :: Matrix a -> [Row a]
cols = transpose --built in function, turn all row into col, vice versa

-- alot of these functions are their own inverse

-- dot operator, function composition
-- h x = f (g x)
-- f . g = h

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
    where 
        pack   = split . map split
        split  = chop boxSize
        unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

{- Brute Force -}
type Choices = [Value]

-- matrix of choices
choices :: Grid -> Matrix Choices
-- choices is one fuzzy value- many
-- replace each empty cell with a list of all poss values
-- we want a list of concrete matricies- matrix with values in it
-- [Matrix Value]
choices g = map (map choice) g
            where
                choice v = if empty v then values else [v]

-- this changes matrix of choices to matrix with values
collapse :: Matrix [a] -> [Matrix a]
collapse = sequence . map sequence
--TODO function sequence
-- map seq over all rows

-- now check if all of the enumerations are correct
solveBrute :: Grid -> [Grid]
solveBrute = filter valid . collapse . choices


{- Prune Second -}

-- need to make better ^ way too slow
-- find which values are already in the row
-- dont need to put every option in each empty box- too many options and some are not valid

prune :: Matrix Choices -> Matrix Choices
-- extract all components, apply reduce function, remap all comp
-- has to do with f . map reduce . f
prune = pruneBy boxes . pruneBy cols . pruneBy rows
        where pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
-- single cell xs - xss is just one row
-- get rid of all the stuff already occured in the col- any singleton cell- need to GET
-- subtract all the singles out of the col
-- 'minus' infix notation- same as minus xs singles
reduce xss = [ xs `minus` singles | xs <- xss]
            where singles = concat (filter single xss)
            -- ret [v], need to concat

--[x + y | x < [1, 2, 3], y <- [4, 5, 6]]
minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

solvePrune :: Grid -> [Grid]
solvePrune = filter valid . collapse . prune . choices

solveFixPrune :: Grid -> [Grid]
solveFixPrune = filter valid . collapse . fix prune . choices

fix :: Eq a => (a -> a) -> a -> a
fix f x =  if x == x' then x else fix f x'
           where x' = f x

{- Refine Third -}

-- still too slow ^

solve :: Grid -> [Grid]
solve = search . prune . choices

search :: Matrix Choices -> [Grid]
search m
    | blocked m   = []
    | complete m = collapse m
    -- some sells w nondeterministic entries
    -- choose one sell, fix each of the options
    -- after fixed, do more pruning
    | otherwise  = [g | m' <- guesses m
                       , g <- search (prune m')]

guesses :: Matrix Choices -> [Matrix Choices]
-- decompost matric into peices
-- get single cell out to manipuate
-- reconstruct the matric w the new choice
guesses m = 
  [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
      -- rows 1 filled, rows atleast 1 not filled
      (rows1, row : rows2) = break (any (not . single)) m
      -- need correct element- do on row itself- find cell itself
      (row1, cs : row2)    = break (not . single) row
      -- list comp to make choice

complete :: Matrix Choices -> Bool
complete = all (all single)

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe cm = all consistent (rows cm) &&
          all consistent (cols cm) &&
          all consistent (boxes cm)

consistent :: Row Choices -> Bool
consistent = noDups . concat . filter single

main :: IO ()
main = (putStrLn . unlines . head . solve) puzzle


-- comp everything is a single, check
-- blcoked one choice empty, or u have an inconsistency
-- guesses make the nondeterminstic choice
