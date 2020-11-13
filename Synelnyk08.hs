
module Synelnyk08 where

import Data.List

data Recur = Zero | Succ | Sel Int Int
           | Super Recur [Recur]
           | Prim Recur Recur
           | Mini Recur Int
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]

-- ������ 1 ------------------------------------
isNumbConst :: System -> Recur -> Bool
isNumbConst _ Zero = True
isNumbConst syst (Super Succ [f]) = isNumbConst syst f
isNumbConst syst (Name f) = isNumbConst syst (findFuncByName syst f)
isNumbConst _ _ = False

findFuncByName :: System -> String -> Recur
findFuncByName  [] str = error ("Function " ++ str ++ " does not exist")
findFuncByName ((name, f):rest) str  | name == str  = f
                                     | otherwise = findFuncByName rest str


-- ������ 2 ------------------------------------
evRank :: System -> Recur -> Int
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = (evRank syst st)-1
evRank syst (Mini b _) = (evRank syst b)-1
evRank syst (Name f) = evRank syst (findFuncByName syst f )

-- ������ 3 ------------------------------------
isNames :: System -> Bool
isNames syst = checkUniqueName (map fst syst) && checkNames syst []

checkNames :: System -> [String] -> Bool
checkNames [] _ = True
checkNames ((name, f):rest) definit = luckyNames && checkNames rest (name:definit)
  where names = allNames f
        contains xs x = elem x xs
        luckyNames = all (contains definit) names

checkUniqueName :: [String] -> Bool
checkUniqueName [] = True
checkUniqueName (x:syst) | elem x syst = False
                         | otherwise = checkUniqueName syst

allNames :: Recur -> [String]
allNames Zero = []
allNames Succ = []
allNames (Sel _ _) = []
allNames (Name f) = [f]
allNames (Mini b _) = allNames b
allNames (Prim g h) = allNames g ++ allNames h
allNames (Super g h) = allNames g ++ concatMap allNames h

-- ������ 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur _ Zero = True
isRecur _ Succ = True
isRecur _ (Sel n k) = (n>=1) && (k>=1) && (n>=k)
isRecur syst (Mini b x) = (isRecur syst b) && (x>=0)
isRecur syst (Prim g h) = (evRank syst g)+2 ==  (evRank syst h) && isRecur syst g && isRecur syst h
isRecur syst (Super g h) = (evRank syst g) == length h && isRecur syst g && all (isRecur syst) h
isRecur syst (Name str) | Just (_,_) <- maybeFuncByName syst str = True
                        | otherwise = False

maybeFuncByName :: System -> String -> Maybe (String,Recur)
maybeFuncByName syst name = find (\x -> name == (fst x)) syst


-- ������ 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int
eval _ (Zero) _ = 0
eval _ (Succ) xs = (head xs) + 1
eval _ (Sel _ k) xs | k<0 || k>(length xs) = 0
                    | otherwise = xs!!(k-1)
eval _ (Mini _ _) _ = 0
eval syst (Super g h) xs = eval syst g [eval syst x xs | x<-h]
eval syst (Prim g h) xs | last xs == 0  = eval syst g xs
                        | otherwise = eval syst h (newList ++ [eval syst (Prim g h) (init newList)])
                        where lastElemMin = [last xs - 1]
                              newList = xs ++ lastElemMin
eval syst (Name str) xs = eval syst (getDefinitionOfFunc syst str) xs

getDefinitionOfFunc :: System -> String -> Recur
getDefinitionOfFunc [] _ = Zero
getDefinitionOfFunc (pair:syst) f | (fst pair)==f = snd pair
                                  | otherwise = getDefinitionOfFunc syst f

-- ������ 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart _ (Zero) _ = Just 0
evalPart _ (Succ) vl = Just (vl!!0 + 1)
evalPart _ (Sel _ k) vl | k<0 || k>(length vl) = Just 0
                        | otherwise = Just (vl!!(k-1))
evalPart syst (Name str) vl = evalPart syst (getDefinitionOfFunc syst str) vl
evalPart _ (Mini _ _) _ = undefined
evalPart _ (Super _ _) _ = undefined
evalPart _ (Prim _ _) _ = undefined


-- ������ 7 ------------------------------------
parseRec :: String -> Maybe System
parseRec = undefined

--------------------- ������ ��� -  -------
syst1, syst2 :: System
syst1 = [("const0", Zero)
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]])
   , ("const2", Super Succ [Super Succ [Zero]])
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ]))
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))
   , ("subtract1", Prim Zero (Sel 2 1))
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)
   ]

syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"

sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
