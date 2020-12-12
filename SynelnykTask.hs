{-# OPTIONS_GHC -Wall #-}
module SynelnykTask where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

type Graph  = [[Int]]

data Tree23 a  = Leaf a
               | Fork2 (Tree23 a) a (Tree23 a)
               | Fork3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Null23     -- порожнє 2-3-дерево!!!
               deriving (Eq, Show)

-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where
   (<=) Zero Zero = True
   (<=) (Pred _) Zero = True
   (<=) Zero (Pred _) = False
   (<=) Zero (Succ _) = True
   (<=) (Succ _) Zero = False
   (<=) (Pred _) (Succ _) = True
   (<=) (Succ _) (Pred _) = False
   (<=) (Succ x) (Succ y) = x <= y
   (<=) (Pred x) (Pred y) = x <= y

-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero = 0
aiToInteger (Succ x) = 1 + (aiToInteger x)
aiToInteger (Pred x) = (aiToInteger x) - 1

-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero Zero = Zero
plusAbs x Zero = x
plusAbs Zero x = x
plusAbs (Pred x) (Succ y) = plusAbs x y
plusAbs (Succ x) (Pred y) = plusAbs x y
plusAbs x (Pred y) = plusAbs (Pred x) y
plusAbs x (Succ y) = plusAbs (Succ x) y

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs _ Zero = Zero
timesAbs ai1@(Pred _) ai2@(Pred _) = timesAbsHelper (negate ai1) (negate ai2)
timesAbs ai1@(Pred _) ai2@(Succ _) = negate (timesAbsHelper (negate ai1) ai2)
timesAbs ai1@(Succ _) ai2@(Succ _) = timesAbsHelper ai1 ai2
timesAbs ai1@(Succ _) ai2@(Pred _) = negate (timesAbsHelper ai1 (negate ai2))

timesAbsHelper :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbsHelper Zero _ = Zero
timesAbsHelper _ Zero = Zero
timesAbsHelper x (Pred y) = timesAbsHelper x y - x
timesAbsHelper x (Succ y) = x + timesAbsHelper x y


-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs

    negate Zero = Zero
    negate (Pred x) = Succ (negate x)
    negate (Succ x) = Pred (negate x)

    fromInteger x | x == 0 = Zero
                  | x < 0  = Pred (fromInteger (x + 1))
                  | otherwise = Succ (fromInteger (x - 1))

    abs x | x < Zero = negate(x)
          | otherwise = x

    signum x | x > Zero = 1
             | x < Zero = -1
             | otherwise = 0

-- Задача 6 -----------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr = if  (not (nodeCycles == [])) then Just (reverse (head nodeCycles)) else Nothing
          where nodeCycles = [c | c <- cycles gr, and [elem n c | n <- nodes gr]]

cycles :: Graph -> [[Int]]
cycles gr = filter (\w -> head w == last w) ([ways | ways <- concat [allWays gr v | v <- nodes gr]])

nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

allWays :: Graph -> Int -> [[Int]]
allWays g v = concat (until cond (oneStep g) [[[v]]])

cond :: ([[[Int]]]) -> Bool
cond wss = null(head wss)

oneStep :: Graph -> [[[Int]]] -> [[[Int]]]
oneStep _ [] = []
oneStep g wss@(wsn:_) = [t:w| w@(x:xs) <- wsn, notElem x xs, t <- g !! x] : wss

-- Задача  7 -----------------------------------------
isAcyclic :: Graph -> Bool
isAcyclic gr = if ((acyclic (cycles gr)) == []) then True else False
    where acyclic cycl = filter (\c -> not((length c) == 1)) cycl

-- Задача 8 -----------------------------------------
isTopolSort :: Graph -> [Int] -> Bool
isTopolSort gr ts = if foldl1 (&&) [(isBe x) && (isContain x (nodes gr)) | x <- gr] then isTopolSortHelper (edges gr) ts
                  else  error "graph is not oriented"

isBe :: [Int] -> Bool
isBe [] = True
isBe (x:xs) | x `elem` xs = False
            | otherwise = isBe xs

isContain :: [Int] -> [Int] -> Bool
isContain [] _ = True
isContain _ [] = False
isContain  a b = foldl1 (&&) [x `elem` b | x <- a]

isTopolSortHelper :: [(Int,Int)] -> [Int] -> Bool
isTopolSortHelper [] [] = True
isTopolSortHelper (_:_) [] = False
isTopolSortHelper ed (x:xs) | not(isMinimum ed x) = False
                            | otherwise = isTopolSortHelper [y | y <- ed, not((fst y) == x)] xs

edges :: Graph -> [(Int,Int)]
edges g = [(x,y)| x <- nodes g, y <- g !! x]

isMinimum :: [(Int,Int)] -> Int -> Bool
isMinimum [] _ = True
isMinimum ed v = foldl1 (&&) [not((snd y) == v)| y <- ed]

-- Задача 9 -----------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr a b | a == b = error "graph must be oriented"
               | otherwise = if (connectedPath gr a b == []) then Nothing else Just (reverse (snd (maximum ([(length x, x) | x <- connectedPath gr a b]))))

connectedPath :: Graph -> Int -> Int -> [[Int]]
connectedPath gr a b = filter (\w -> (w !! 0) == b) (simpleWays gr a)

simpleWays :: Graph -> Int -> [[Int]]
simpleWays g v = filter (\p -> path p) (allWays g v)

path :: [Int] -> Bool
path p = and [length (filter (/=x) p) == (length p - 1) | x <- p]

--- Задача 10 ----------------------------------------
merge :: [Int] -> [Int] -> [Int]
merge s1 s2 = unique(concat[s1, s2])

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)  | x `elem` xs  = unique xs
               | otherwise = x : unique xs


--- Задача 11 ----------------------------------------
--intToString 543 16  = “21f”
--intToString 543 6 = “2303”

intToString :: Int -> Int -> String
intToString n m
                | m > 16 = "bigger than we can convert"
                | otherwise = concat [useLetters num | num <- convert n m]

useLetters :: Int -> String
useLetters x
    | x == 10 = "a"
    | x == 11 = "b"
    | x == 12 = "c"
    | x == 13 = "d"
    | x == 14 = "e"
    | x == 15 = "f"
    | otherwise = show x

convert :: Int -> Int -> [Int]
convert 0 _ = []
convert n m = concat [(convert (n `div` m) m), [n `mod` m]]

--- Задача 12 ----------------------------------------
--stringToInt 10 "56a" = Nothing
--stringToInt 16 "21f" = Just 543
stringToInt :: Int -> String -> Maybe Int
stringToInt n xs | notBecome n xs = Nothing
                 | otherwise = Just (calculate n xs (length xs - 1))

findInt :: Char -> Int
findInt x
    | x == '0' = 0
    | x == '1' = 1
    | x == '2' = 2
    | x == '3' = 3
    | x == '4' = 4
    | x == '5' = 5
    | x == '6' = 6
    | x == '7' = 7
    | x == '8' = 8
    | x == '9' = 9
    | x == 'a' = 10
    | x == 'b' = 11
    | x == 'c' = 12
    | x == 'd' = 13
    | x == 'e' = 14
    | x == 'f' = 15
    | otherwise = 0


calculate :: Int -> String -> Int -> Int
calculate n xs m | (m == 0) = findInt (head xs)
                 | otherwise = (findInt (head xs)) * (n^m) + (calculate n (tail xs) (m - 1))

notBecome :: Int -> String -> Bool
notBecome n xs = or [(findInt x) >= n | x<-xs]

--- Задача 13 ----------------------------------------
genExpr ::  Int -> Int -> [String]
genExpr a b = [fst ex | ex <- genExprHelper(reverse(convertOne a)), snd ex == b]

genExprHelper :: [Int] -> [(String, Int)]
genExprHelper [] = [([],0)]
genExprHelper [x] = [(show x, x)]
genExprHelper (x:xs) = concat [[( concat[fst(ex), ('*': show x)], (snd ex) * x ) | ex <- genExprHelper xs],
                              [( concat[fst(ex), ('+': show x)], (snd ex) + x ) | ex <- genExprHelper xs],
                              [( concat[fst(ex), ('-': show x)], (snd ex) - x ) | ex <- genExprHelper xs]]

convertOne :: Int -> [Int]
convertOne 0 = []
convertOne x = concat[convertOne (x `div` 10), [x `mod`10]]

--- Задача 14 ----------------------------------------
genExprBracket :: Int -> Int -> [String]
genExprBracket = undefined

--- Задача 15 ----------------------------------------
--isTree23 (Fork2 (Leaf 0) 1 (Leaf 2)) =False
--isTre23 tr3 = True (error)
isTree23  :: (Ord a) => Tree23 a -> Bool
isTree23 (Null23) = True
isTree23 (Leaf _) = True
isTree23 (Fork2 tl x tr) = (x >= maxInTree tl) && (x == minInTree tr)
isTree23 (Fork3 tl x tm y tr) = (x >= maxInTree tl) && (y >= maxInTree tm) && (x == minInTree tm) && (y == minInTree tr)

minInTree :: Tree23 a -> a
minInTree (Leaf x) = x
minInTree (Fork2 Null23 x _) = x
minInTree (Fork2 tl _ _) = minInTree tl
minInTree (Fork3 Null23 x _ _ _) = x
minInTree (Fork3 tl _ _ _ _) = minInTree tl

maxInTree :: Tree23 a -> a
maxInTree (Leaf x) = x
maxInTree (Fork2 _ x Null23) = x
maxInTree (Fork2 _ _ tr) = maxInTree tr
maxInTree (Fork3 _ _ _ x Null23) = x
maxInTree (Fork3 _ _ _ _ tr) = maxInTree tr

--- Задача 16 ----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 a b = treeValues a == treeValues b

-- treeValues (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
--[16,18,19]
treeValues :: Tree23 a -> [a]
treeValues Null23 = []
treeValues (Leaf x) = [x]
treeValues (Fork2 tl _ tr) = concat[treeValues tl, treeValues tr]
treeValues (Fork3 tl _ tm _ tr) = concat[treeValues tl, treeValues tm, treeValues tr]

--- Задача 17 ----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Null23 _ = False
elemTree23 (Leaf x) v = x == v
elemTree23 (Fork2 tl x tr) v | (v == x) = True
                             | (v < x) = elemTree23 tl v
                             | (v > x) = elemTree23 tr v
                             |otherwise = False
elemTree23 (Fork3 tl x tm y tr) v | (v == x) = True
                                  | (v == y) = True
                                  | (v < x) = elemTree23 tl v
                                  | (v < y) = elemTree23 tm v
                                  | (v > y) = elemTree23 tr v
                                  | otherwise = False

--- Задача 18 ----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23  = undefined

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Fork2 (Leaf _) _ _)     = True
isTerminal (Fork3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево,
--   корінь якого - вузол вида Fork2 або Fork3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insFork v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insFork v tr - додає значення v в дерево tr з корнем - нетермінальний вузол
insFork :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insFork = undefined

---------------------Тестові дані

---------------------- Графи -------
gr1, gr2, gr3:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]

---------------------- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Fork2 (Fork2 (Fork2 (Leaf 0) 1 (Leaf 1))
                     2
                    (Fork2 (Leaf 2) 3 (Leaf 3)))
              4
             (Fork2 (Fork2 (Leaf 4) 5 (Leaf 5))
                     6
                    (Fork2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Fork3 (Fork2 (Leaf 0) 1 (Leaf 1))
              2
             (Fork3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Fork3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork2 (Leaf 16) 19 (Leaf 19))

tr4 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Fork2 (Fork2 (Fork2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Fork2 (Leaf 7) 8 (Leaf 8))
            )
            10
            (Fork2 (Fork2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )