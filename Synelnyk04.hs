
module Synelnyk04 where

import Data.Char(isDigit, digitToInt)

-- Задача 1 -----------------------------------------
analyseG :: String -> Bool
analyseG st = case s st of
  Just st1 -> null st1
  Nothing  -> False

s :: String -> Maybe String
s ('a' : st1) = case s st1 of
  Just ('b' : st2) -> case a st2 of
    Just ('a' : st3) -> Just st3
    _ -> Nothing
  _ -> Nothing
s ('b' : st1) = Just st1
s _ = Nothing

a :: String -> Maybe String
a ('b' :('a' : st1)) = case a st1 of
   Just st2 -> s st2
   _ -> Nothing
a ('a' : st1) = Just st1
a _ = Nothing

-- Задача 2 ----------------------------------------
balance :: String -> Bool
balance str = case b str of
  Just st1 -> null st1
  Nothing -> False

b :: String -> Maybe String
b str = e (c str)

c :: String -> String
c (' ':st1) = c st1
c str = str

e :: String -> Maybe String
e ('(':st1) = case b st1 of
          Just (')':st2) -> b st2
          _ -> Nothing
e ('{':st1) = case b st1 of
          Just ('}':st2) -> b st2
          _ -> Nothing
e ('[':st1) = case b st1 of
          Just (']':st2) -> b st2
          _ -> Nothing
e str = Just str


-- Задача 3 -----------------------------------------
analyseExpr :: String -> Bool
analyseExpr st = case ae st of
    Just st1 -> null st1
    _ -> False

ae :: String -> Maybe String
ae st = case af st of
    Just st1 -> aa st1
    Nothing -> Nothing

aa :: String -> Maybe String
aa (p:st) | elem p "+-*" = case af st of
    Just st1 -> aa st1
    Nothing -> Nothing
aa st = Just st

af :: String -> Maybe String
af ('(':st) = case ae st of
    Just (')':st1) -> Just st1
    _ -> Nothing
af (d:st) | isDigit d = Just st
af  _ = Nothing

-- Задача 4 -----------------------------------------
evalLeft :: String -> Maybe Int
evalLeft st = case le st of
    Just (v,st1) | null st1 -> Just v
    _  -> Nothing

le :: String -> Maybe (Int,String)
le st = case lf st of
    Just (v1,st1) -> la (v1, st1)
    Nothing -> Nothing

la :: (Int,String) -> Maybe (Int,String)
la (v1, d : st)| d `elem` "+-*" = case lf st of
    Just (v2,st1) -> la (inOp d v1 v2, st1)
    Nothing -> Nothing
la (v1,st) = Just (v1,st)

lf :: String -> Maybe (Int,String)
lf ('(':st) = case le st of
    Just (v,(')':st1)) -> Just (v,st1)
    _ -> Nothing
lf (d:st) | isDigit d = Just (digitToInt d, st)
lf  _ = Nothing

-- Задача 5 -----------------------------------------
evalRigth :: String -> Maybe Int
evalRigth st = case re st of
    Just (v,st1) | null st1 -> Just v
    _  -> Nothing

re :: String -> Maybe (Int,String)
re st = case rf st of
    Just (v1,st2) -> ra (v1, st2)
    Nothing -> Nothing

ra :: (Int,String) -> Maybe (Int,String)
ra (v1, d : st)| d `elem` "+-*" = case re st of
    Just (v2,st1) -> ra (inOp d v1 v2, st1)
    Nothing -> Nothing
ra (v1,st) = Just (v1,st)

rf :: String -> Maybe (Int,String)
rf ('(':st) = case re st of
    Just (v,(')':st1)) -> Just (v,st1)
    _ -> Nothing
rf (d:st) | isDigit d = Just (digitToInt d, st)
rf  _ = Nothing

-- Задача 6 -----------------------------------------
evalPrior :: String -> Maybe Int
evalPrior st = case pe st of
    Just (v,st1) | null st1 -> Just v
    _ -> Nothing

pe :: String -> Maybe (Int, String)
pe st = case pt st of
    Just (v1,st1) -> pa (v1, st1)
    Nothing -> Nothing

pa :: (Int,String) -> Maybe (Int,String)
pa (v1, d : st)| d `elem` "+-" = case pt st of
    Just (v2,st1) -> pa (inOp d v1 v2, st1)
    Nothing -> Nothing
pa (v1,st) = Just (v1,st)

pt :: String -> Maybe (Int, String)
pt st = case pf st of
    Just (v1,st1) -> pb (v1, st1)
    Nothing -> Nothing

pb :: (Int,String) -> Maybe (Int,String)
pb (v1, d : st)| d `elem` "*" = case pf st of
    Just (v2,st1) -> pb (inOp d v1 v2, st1)
    Nothing -> Nothing
pb (v1,st1) = Just (v1,st1)

pf :: String -> Maybe (Int, String)
pf ('(':st) = case pe st of
    Just (v,(')': st1)) -> Just (v,st1)
    _ -> Nothing
pf (d:st) | isDigit d = Just (digitToInt d, st)
pf  _ = Nothing

--------------------------------------------------------

inOp :: Char -> Int -> Int -> Int
inOp c2 = case c2 of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}