{-# OPTIONS_GHC -Wall #-}
module Synelnyk03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving (Show, Eq)
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool
isPrefix _ [] = False
isPrefix [] _ = True
isPrefix bs xs = if (bs==xs) then isPrefix bs xs else False

-- Задача 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (ls, rs, _) i str = take i str ++ rs ++ drop (i + length ls) str

-- Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)]
findPosition str (ls, rs, e) = [((ls,rs,e),x) | x <- [0 .. length str], isPrefix ls (drop x str)]

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]
findAll [] _ = []
findAll algo str = findPosition str (head algo) ++ findAll (tail algo) str

-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA _ (False, st, word) = (False, st, word)
stepA algo (_, st, word) =
    let ((ls,rs,e), i) = head (findAll algo word)
    in (not e, st + 1, substitute (ls, rs, e) i word)

-- Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String
evalA algo m word =
    let (bt, st, wr) = until cond step (True, 0, word)
                            where step :: ConfigA -> ConfigA
                                  step (b, s, w) = stepA algo (b, s, w)
                                  cond :: ConfigA -> Bool
                                  cond (bc, nc, _) = (bc == False || nc >= m)
    in if ((bt == True) && (st >= m)) then Nothing else Just wr

-- Задача 7 ------------------------------------
maximReg :: Program -> Int
maximReg [] = 0
maximReg (x:pr) = max (findMaxReg x 0) (maximReg pr)
    where findReg :: Command -> Int -> Int
          findReg (Z a) cur = max a cur
          findReg (S a) cur = max a cur
          findReg (T a b) cur = max (max a b) cur
          findReg (J a b _) cur = maximum ([cur,b,a])

-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int]
ini [] _ = []
ini pr ir =
    let lengthMax = maximReg pr
    in if (lengthMax >= (length ir)) then ir ++ (take ((lengthMax) - (length ir))[0,0..]) else ir

upd :: [Int] -> Int -> Int-> [Int]
upd [] _ _ = []
upd reg r v = (take r reg) ++ v:(drop (r+1) reg)

-- Задача 9 ------------------------------------
stepC :: Program -> ConfigC -> ConfigC
stepC pr (nm, st, rg) =
            let putCommand :: Command -> ConfigC -> ConfigC
                putCommand (Z a) (nm1, st1, rg1) = (nm1+1, st1+1, upd rg1 (a-1) 0)
                putCommand (S a) (nm1, st1, rg1) = (nm1+1, st1+1, upd rg1 (a-1) ((rg1!!(a-1))+1))
                putCommand (T a b) (nm1, st1, rg1) = (nm1+1, st1+1, upd rg1 (b-1) (rg1!!(a-1)))
                putCommand (J a b q) (nm1, st1, rg1) = if (rg1!!(a-1)) == (rg1!!(b-1)) then (q, st1+1, rg1) else (nm1+1,st1+1,rg1)
            in  putCommand (pr!!(nm-1)) (nm, st, rg)

-- Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int
evalC [] _ _ = Nothing
evalC pr mx ir =
    let (nm, st, rg) = until cond step (1, 0, (ini pr ir))
                            where cond :: ConfigC -> Bool
                                  cond (n, s, _) = (s >= mx || (n > length pr))
                                  step :: ConfigC -> ConfigC
                                  step c = stepC pr c
    in if ((st >= mx) && nm <= (length pr)) then Nothing else Just (head rg)

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ]

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ]
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False)
          , ("db", "bd", False)
          , ("d", "", True)
          , ("caa", "aca", False)
          , ("cab", "bca", False)
          , ("cba", "acb", False)
          , ("cbb", "bcb", False)
          , ("", "c", False)
          ]

-- добуток натуральних чисел
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1]

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
