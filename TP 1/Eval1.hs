module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = [("t",0),("n",0),("i",0)]

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
evalComm :: Comm -> State -> State
evalComm Skip               s = s
evalComm (Let v i)          s = update v (fst $ evalIntExp i s) s
evalComm (Seq x y)          s = let s' = evalComm x s
                                in evalComm y s'
evalComm (IfThenElse b x y) s = case (evalBoolExp b s) of
                                  (True, s') -> evalComm x s'
                                  (_ ,   s') -> evalComm y s'
evalComm (While b c)        s = case (evalBoolExp b s) of
                                  (True, s') -> let s2 = evalComm c s'
                                                in evalComm (While b c) s2
                                  (_,    s') -> s'
 
-- Evalua una expresion entera, con efectos laterales

binop :: (Integer -> Integer -> b) -> IntExp -> IntExp -> State -> (b, State) 
binop f x y s = let (x', s1) = evalIntExp x s
                    (y', s2) = evalIntExp y s1
                in (f x' y' , s2)

evalIntExp :: IntExp -> State -> (Integer, State)
evalIntExp (Const c)      s = (c, s)
evalIntExp (Var x)        s = (lookfor x s, s)
evalIntExp (UMinus x)     s = binop (*) x (Const (-1)) s
evalIntExp (Plus  x y)    s = binop (+) x y s
evalIntExp (Minus x y)    s = binop (-) x y s
evalIntExp (Times x y)    s = binop (*) x y s
evalIntExp (Div   x y)    s = binop div x y s

evalIntExp (Assign v i)   s = let (i' , s1) = evalIntExp i s
                              in (i', update v i' s1)
evalIntExp (Secuence x y) s = let (x', s') = evalIntExp x s
                              in evalIntExp y s'


-- Evalua una expresion booleana, con efectos laterales

binopBool :: (Bool -> Bool -> b) -> BoolExp -> BoolExp -> State -> (b, State) 
binopBool f x y s = let (x', s1) = evalBoolExp x s
                        (y', s2) = evalBoolExp y s1
                    in (f x' y' , s2)

evalBoolExp :: BoolExp -> State -> (Bool, State)
evalBoolExp BTrue     s = (True,s)
evalBoolExp BFalse    s = (False,s)
evalBoolExp (Eq x y)  s = binop (==) x y s
evalBoolExp (NEq x y) s = binop (/=) x y s
evalBoolExp (Lt x y)  s = binop (<) x y s
evalBoolExp (Gt x y)  s = binop (>) x y s
evalBoolExp (And x y) s = binopBool (&&) x y s
evalBoolExp (Or x y)  s = binopBool (||) x y s
evalBoolExp (Not x)   s = let (b, s') = (evalBoolExp x s)
                          in (not b, s')
