import Data.List

{- Types -}

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

{- Constants -}

boxSize :: Int
boxSize = 3

values :: [Value]
values = ['1' .. '9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _   = False

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

{- Checking Correctness -}

valid :: Grid -> Bool
valid g = all noDups (rows g) &&
          all noDups (cols g) &&
          all noDups (boxes g)

noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (x : xt) = not (elem x xt) && noDups xt

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
        where
          pack   = split . map split
          split  = chop boxSize
          unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

{- Brute First -}

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices g = map (map choice) g
            where
              choice v = if empty v then values else [v]

collapse :: Matrix [a] -> [Matrix a]
collapse =  sequence . map sequence

solveBrute :: Grid -> [Grid]
solveBrute =  filter valid . collapse . choices

{- Prune Second -}

prune :: Matrix Choices -> Matrix Choices
prune =  pruneBy boxes . pruneBy cols . pruneBy rows
         where pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss =  [xs `minus` singles | xs <- xss]
              where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

solvePrune :: Grid -> [Grid]
solvePrune = filter valid . collapse . prune . choices

{- Refine Third -}

solve :: Grid -> [Grid]
solve = search . prune . choices

search :: Matrix Choices -> [Grid]
search m
  | blocked m  = []
  | complete m = collapse m
  | otherwise  = [g | m' <- guesses m
                    , g  <- search (prune m')]

guesses :: Matrix Choices -> [Matrix Choices]
guesses m =
  [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = break (any (not . single)) m
    (row1, cs : row2)    = break (not . single) row

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

{- Main -}

main :: IO ()
main = (putStrLn . unlines . head . solve) puzzle
