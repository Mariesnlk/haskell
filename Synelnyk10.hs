
module Synelnyk10 where

-- розглядаємо лише цілі дані: скаляри  і масиви
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int
         | Var Id
         | OpApp Op Exp Exp
         | Cond Exp Exp Exp
         | FunApp Id [Exp]
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp
          | If Exp Stmt Stmt
          | While Exp Stmt
          | Call Id [Exp]
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b [] = [(a,b)]
updateValue a b ((a1, b1):abss) | a1 == a = concat[[(a, b)], abss]
                                | otherwise = concat[[(a1, b1)], updateValue a b abss]

-- Задача 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A a) (I i) (I v) = A (updateValue i v a)
updateArray _ _ _ = error ("Function does not exist")

-- Задача 3 ------------------------------------
applyOp :: Op -> Value -> Value -> Value
applyOp Add (I v1) (I v2) = I (v1 + v2)
applyOp Minus (I v1) (I v2) = I (v1 - v2)
applyOp Mul (I v1) (I v2) = I (v1 * v2)
applyOp Less (I v1) (I v2) | v1 > v2 = I 0
                           | otherwise = I 1

applyOp Equal (I v1) (I v2) | v1 == v2 = I 1
                            | otherwise = I 0

applyOp Index (A ((a1, b1):v1)) (I v2) | a1 == v2 = I b1
                                       | otherwise = applyOp Index (A v1) (I v2)

applyOp _ _ _ = I 0

-- Задача 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value
evExp (Const c) _ _ = I c
evExp (Var e) dfx ((x, val):st) | x == e = val
                                | otherwise = evExp (Var e) dfx st
evExp (Var _) _ [] = I 0
evExp (OpApp op e1 e2) dfx st = applyOp op (evExp e1 dfx st) (evExp e2 dfx st)
evExp (Cond con e1 e2) dfx st | (evExp con dfx st) /= I 0 = evExp e1 dfx st
                              | otherwise = evExp e2 dfx st

evExp (FunApp f es) dfx st = evExp ef dfx new
                              where (as, ef) = lookUp f dfx
                                    vs = evArgs es dfx st
                                    new = zip (map getId as) vs

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]
evArgs ex dfx st = [evExp x dfx st | x<-ex]

getId :: VarDef -> Id
getId (Arr i) = i
getId (Int i) = i

-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign i expr) dfx _ st = updateValue i (evExp expr dfx st) st
evStmt (AssignA i exp1 exp2) dfx _ st = updateValue i (updateArray (lookUp i st) (evExp exp1 dfx st) (evExp exp2 dfx st)) st
evStmt (If expr stmt1 stmt2) dfx dpx st | (evExp expr dfx st /= I 0) = evStmt stmt1 dfx dpx st
                                        | otherwise = evStmt stmt2 dfx dpx st

evStmt (While expr stmt) dfx dpx st = until (isExist expr dfx) (evStmt stmt dfx dpx) st
evStmt (Call _ _) _ _ _ = undefined
evStmt (Block _ _) _ _ _ = undefined

isExist :: Exp -> [FunDef] -> StateP -> Bool
isExist expr dfx st = if evExp expr dfx st == (I 0) then True else False

-- Задача 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
iswfExp (Const _) _ _ = Just It
iswfExp (Var e) ve _ = lookup e ve
--iswfExp e ve fe = undefined
iswfExp (OpApp op e1 e2) ve fe | (Just a, Just b) <- (iswfExp e1 ve fe, iswfExp e2 ve fe) = iswfOp op [a,b]
                               | otherwise = Nothing

iswfExp (Cond con e1 e2) ve fe | (Just a1, Just b1, Just c1) <- (a, b, c) = iswfCond [a1, b1, c1]
                               | otherwise = Nothing
                               where a = iswfExp con ve fe
                                     b = iswfExp e1 ve fe
                                     c = iswfExp e2 ve fe

iswfExp (FunApp f es) ve fe | map Just getExpVal == map (\x -> iswfExp x ve fe) es = Just It
                            | otherwise = Nothing
                            where getExpVal = lookUp f fe

-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign i ex) ve fe _ =  lookup i ve == iswfExp ex ve fe &&  lookup i ve ==  Just It
iswfStmt (AssignA i exp1 exp2) ve fe _ = iswfAssignA [att, itt1, itt2]
                                        where Just att = Just (lookUp i ve)
                                              Just itt1 = iswfExp exp1 ve fe
                                              Just itt2 = iswfExp exp2 ve fe

iswfStmt (If expr stmt1 stmt2) ve fe pe = Just It == iswfExp expr ve fe && st1 && st2
                                          where st1 = iswfStmt stmt1 ve fe pe
                                                st2 = iswfStmt stmt2 ve fe pe

iswfStmt (While ex st) ve fe pe = Just It == e && sm
                                 where e = iswfExp ex ve fe
                                       sm = iswfStmt st ve fe pe
iswfStmt (Call _ _) _ _ _ = undefined
iswfStmt (Block _ _) _ _ _ = undefined

-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef = undefined

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef = undefined

-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram = undefined


--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx)

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0)

-- Реалізація виконання програми
evProgram :: Program -> StateP
evProgram (dvx, dfx, dpx) =
   let sb = map initv dvx
       ( _, s) = lookUp "main" dpx
   in  evStmt s dfx dpx sb

--  iswfOp o ts - перевіряє коректність типів операндів ts
--     бінарної операції o і формує тип результату Just t або Nothing
iswfOp :: Op -> [Type] -> Maybe Type
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing
iswfCond :: [Type] -> Maybe Type
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True
iswfAssignA _          = False

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"],
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"],
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0))
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку
sampleBlock :: Stmt
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y
gAdd :: ProcDef
gAdd = ("gAdd",
        ([Int "x", Int "y"],
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"],
          Block [Int "i", Int "limit"]
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block []
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum;
-- proc gAdd(x,y) gSum = x + y
-- proc main() call gAdd(5,10)
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... }
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [],
       [sumA1,
        ("main",([], sampleBlock))
       ])
