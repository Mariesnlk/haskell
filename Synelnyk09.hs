
module Synelnyk09 where

import Data.List
--import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Done Задача 1 -----------------------------------------
simplify :: RE -> RE
simplify Null = Null
simplify (Term t) = Term t
simplify (Seq r1 r2) = Seq (simplify r1) (simplify r2)
simplify (Alt r1 r2) = Alt (simplify r1) (simplify r2)
simplify (Rep r) = Rep (simplify r)
simplify (Plus r) = Seq (simplify r) (Rep (simplify r))
simplify (Opt r) = Alt (simplify r) (Null)

-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool
isTerminal aut s = elem s (getEnds aut)

isEssential :: Automation -> State -> Bool
isEssential aut s | isExistStateForC (getTrans aut) s  = True
                  |otherwise = isTerminal aut s

getEnds :: Automation -> [State]
getEnds (_, ends, _) = ends

getTrans :: Automation -> [Transition]
getTrans (_, _, trans) = trans

getStart :: Automation -> State
getStart (start, _, _) = start

isExistStateForC :: [Transition] -> Int -> Bool
isExistStateForC [] _ = False
isExistStateForC ((st1, _, C _):t) stat | (st1 == stat) = True
                                  |otherwise = isExistStateForC t stat
isExistStateForC ((_, _, _):t) stat = isExistStateForC t stat

-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom aut s = createListForState (getTrans aut) s

createListForState :: [Transition] -> State -> [Transition]
createListForState [] _ = []
createListForState ((st1, st2, lab):t) s | (st1 == s) = concat[(createListForState t s),[(st1, st2, lab)]]
                                         | otherwise = createListForState t s

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels trx = nub[labl | (_, _, labl) <- trx, labl /= Eps]

-- labels [(5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps)] -> [C 'a',C 'c']

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA daut st = isEntered daut (getStart daut) st

isEntered:: Automation -> State -> String -> Bool
isEntered aut stat [] = elem stat (getEnds aut)
isEntered aut stat (s:str) = if elem stat (getEnds aut) then False else case fun of
                             Just (_,st2,_) -> isEntered aut st2 str
                             Nothing -> False
  where fun = find (\x -> isLabel x stat s) (getTrans aut)

isLabel :: Transition -> Int -> Char -> Bool
isLabel (st1, _, C c) st sym = (st1 == st) && (c == sym)
isLabel (_, _, _) _ _ = False

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

stStep  naut st mc = [fin | (start, fin, labl) <- getTrans naut, mc == labl, st == start]
setStep naut bs mc = concatMap (\stat -> stStep naut stat mc) bs
closure naut ss = sort (nub (concat[(closureAttended naut ss []), ss]))

closureAttended :: Automation -> [State] -> [State] -> [State]
closureAttended _ [] attended = attended
closureAttended aut st attended = closureAttended aut filtr (concat[filtr, attended])
                                where filtr = filter (\x -> notElem x attended) (setStep aut st Eps)

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut st = isAccepts aut st [getStart aut]

--isAccepts ndaFigure "ac" ([1, 6, 9]) -> True
isAccepts :: Automation -> String -> [State] -> Bool
isAccepts _ _ [] = False
isAccepts aut [] ss = elem True (map (isTerminal aut) ss)
isAccepts aut (c:str) ss = isAccepts aut str (setStep aut (closure aut ss) (C c))

-- Done Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null beg fin nxt = ([(beg, fin, Eps)], nxt)
make (Term c) beg fin nxt = ([(beg, fin, C c)], nxt)
make (Seq r1 r2) beg fin nxt  = (concat[tr2++tr1, [(nxt, nxt+1,Eps)]], nxt1)
                                where (tr1, nxt1) = make r2 (nxt+1) fin nxt2
                                      (tr2, nxt2) = make r1 beg nxt (nxt+2)

make (Alt r1 r2) beg fin nxt = (concat[tr1++tr2,[(beg,nxt,Eps), (beg,nxt+2,Eps), (nxt+1,fin,Eps), (nxt+3,fin,Eps)]], nxt2)
                                where (tr1, nxt1) = make r1 nxt (nxt+1) (nxt+4)
                                      (tr2, nxt2) = make r2 (nxt+2) (nxt+3) nxt1

make (Rep r1) beg fin nxt = (concat[tr1, [(beg,fin,Eps), (beg,nxt,Eps), (nxt+1,fin,Eps), (nxt+1,nxt,Eps)]],nxt1)
                                where (tr1, nxt1) = make r1 nxt (nxt+1) (nxt+2)

make (Plus r) beg fin nxt = make (simplify r) beg fin nxt
make (Opt r) beg fin nxt =  make (simplify r) beg fin nxt

-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE
parseReg = undefined

-- Задача 10 -----------------------------------------
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined

makeDA :: Automation -> Automation
makeDA  = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps),
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)])
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
