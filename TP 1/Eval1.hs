module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Integer
lookfor var state = snd $ head $ filter (\(v,i) -> v==var) state

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update var int state = map (\(v,i) -> if (v == var) then (var, int) else (v,i)) state

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip               s = s
evalComm (Let v i)          s = update v (evalIntExp i s) s
evalComm (Seq x y)          s = let s' = evalComm x s
                                in evalComm y s'
evalComm (IfThenElse b x y) s = case (evalBoolExp b s) of
                                  True      -> evalComm x s
                                  otherwise -> evalComm y s
evalComm (While b c)        s = case (evalBoolExp b s) of
                                 True      -> evalComm c s
                                 otherwise -> s

--data Comm = Skip
--          | Let Variable IntExp
--          | Seq Comm Comm
--          | IfThenElse BoolExp Comm Comm
--          | While BoolExp Comm 
-- deriving Show
 
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion

evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const c)      _ = c
evalIntExp (Var x)        s = lookfor x s
evalIntExp (UMinus x)     s = (-1) * (evalIntExp x s)
evalIntExp (Plus  x y)    s = (evalIntExp x s) + (evalIntExp y s)
evalIntExp (Minus x y)    s = (evalIntExp x s) - (evalIntExp y s)
evalIntExp (Times x y)    s = (evalIntExp x s) * (evalIntExp y s)
evalIntExp (Div   x y)    s = div (evalIntExp x s) (evalIntExp y s)
evalIntExp (Assign v i)   s = evalIntExp i s  
evalIntExp (Secuence x y) s = evalIntExp y s

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue     _ = True
evalBoolExp BFalse    _ = False
evalBoolExp (Eq x y)  s = (evalIntExp x s) == (evalIntExp y s)
evalBoolExp (NEq x y) s = (evalIntExp x s) /= (evalIntExp y s)
evalBoolExp (Lt x y)  s = (evalIntExp x s) < (evalIntExp y s)
evalBoolExp (Gt x y)  s = (evalIntExp x s) > (evalIntExp y s)
evalBoolExp (And x y) s = (evalBoolExp x s) && (evalBoolExp y s)
evalBoolExp (Or x y)  s = (evalBoolExp x s) || (evalBoolExp y s)
evalBoolExp (Not x)   s = not (evalBoolExp x s)
