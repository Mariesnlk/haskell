
module Synelnyk05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String
addOne st c | elem c st = st
            | otherwise = sort (st ++ [c])

addAll :: String -> String -> String
addAll st wd = foldl addOne st wd

addWithout :: String -> String -> String
addWithout st wd = addAll st $ filter (/='$') wd

inter :: String -> String -> String
inter st1 st2 = sort $ filter (\a->elem a st2) st1

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String
tkPredict [] _ = []
tkPredict (p:ps) n | n==(fst p) = snd p
                   | otherwise = tkPredict ps n

upPredict :: Predict -> Char -> String -> Predict
upPredict = undefined

-- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gr ctl word = case step gr ctl (word++['$'],(notTerm ctl):['$'],Just []) of
 (_,_,res) -> res

step :: Grammar -> Control ->
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step gr ctl ((x:xs),(y:ys), Just result) |isUpper y= case findResult ctl y x of
                                          Just res -> step gr ctl (x:xs,(snd $ gr !! res)++ys,Just (result++[res]))
                                          Nothing -> ([],[],Nothing)
                                         | y==x = step gr ctl (xs,ys,Just result)
                                         | otherwise = ([],[],Nothing)
step _ _ ([],[],result) = ([],[], result)
step _ _ _ = ([],[],Nothing)

findResult :: Control -> Char->Char -> Maybe Int
findResult [] _ _ = Nothing
findResult (x:rest) st inp | (st ==(fst $ fst x)) && ((snd $ fst x) ==inp) =Just (snd x)
                           | otherwise = findResult rest st inp

notTerm  :: Control -> Char
notTerm  (x:xs) | snd x == 0 = fst $ fst x
 | otherwise = notTerm  xs
notTerm  [] = ' '

-- ? Задача 4 ------------------------------------
first :: Predict -> String -> String
first _ [] = ['$']
first pFst (x:xs) | isUpper x = let res = tkPredict pFst x
                  in if elem '$' res then addWithout (first pFst xs) res else res
                 | x=='$' = first pFst xs
                 | otherwise = [x]

-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control
buildingControl gr pFst pNxt = sort $buildCtr (zip gr [0..]) pFst pNxt

buildCtr::[((Char,String),Int)] ->Predict -> Predict->  Control
buildCtr [] _ _ = []
buildCtr (((x,pr),i):rest) pFst pNxt= let res=first pFst pr
                                          extRes = tkPredict pNxt x
 in if elem '$' res then (makeControl x (addWithout extRes res) i)++buildCtr rest pFst pNxt else (makeControl x res i)++buildCtr rest pFst pNxt

makeControl:: Char ->String-> Int -> Control
makeControl nt wd i= map (\t -> ((nt,t),i)) wd

-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gr pFst pNxt = if notElem False (ar (fromGrammar gr)) then True else False
                         where ar =  map (\x -> (if testFst [first pFst f | f <- snd x] && if elem "" (snd x)  then testFollow (tkPredict pNxt (fst x)) (snd x) else True  then True else False))

uniqueChar :: [Char] -> [Char]
uniqueChar [] = []
uniqueChar (x : xs) = x : uniqueChar (filter (x /=) xs)

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar gr =  [ (notTermAr!!n, [snd g | g <- gr, fst g == notTermAr!!n]) | n<-[0..(length notTermAr-1)] ]
                  where notTermAr = uniqueChar [fst x |x<-gr]

testFst :: [String] -> Bool
testFst rls =   length ls == length (filter (=="") ls)
                where ls = [inter (rls!!a) (rls!!b) |a<- [0..(length rls-1)], b<- reverse [0..(length(rls)-1)], a/=b]


testFollow :: String -> [String] -> Bool
testFollow fs rls =  length ls == length (filter (=="") ls)
                     where ls = [inter a fs |a<- rls ]

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict
buildFst [] = []
buildFst gr = let now = fst (gr!!0)
              in sort((evalFst gr now) ++ buildFst [(fst (gr!!n),snd (gr!!n))|n<-[0..(length gr - 1)], (fst (gr!!n))/=now])

evalFst :: Grammar -> Char -> Predict
evalFst gr now = [(now, sort (extandFst gr now [b|(a,b)<-gr, a==now]))]

extandFst :: Grammar->Char->[String]-> String
extandFst _ _ [] = []
extandFst gr x ("":ys) = "$" ++ extandFst gr x ys
extandFst gr x (y:ys) | isUpper (head y) = let (_,b) = (evalFst gr (head y))!!0
                                           in if elem '$' b then b ++ extandFst gr x ([tail y] ++ ys)
                                           else b ++ extandFst gr x ys
                      | otherwise = [head y] ++ extandFst gr x ys


-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict
buildNxt = undefined

nontermTails :: Grammar -> [(Char,String)]
nontermTails = undefined

evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
evalNxt = undefined

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne = undefined

---------------------Тестові дані ---------------------------

gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")]
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"),
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"),
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]

-- управляючі таблиці
ctl0, ctl1, ctl2 :: Control
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

