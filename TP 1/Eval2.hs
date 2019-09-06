module Eval2 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Error
data Error = DivByZero | UndefVar deriving Show

-- Estado nulo
initState :: State
initState = [("t",0),("n",0),("i",0),("a",0)]

-- Busca el valor de una variabl en un estado
lookfor :: Variable -> State -> Either Error Integer
lookfor var state = let l = filter (\(v,i) -> v==var) state
                    in if (length l == 0) then Left UndefVar
                                          else Right $ snd $ head l

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> Either Error State
update var int state = case (lookfor var state) of
        Right _   -> Right $ map (\(v,i) -> if (v == var) then (var, int) else (v,i)) state
        otherwise -> Left UndefVar

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
continue :: Comm -> Either Error State -> Either Error State
continue cmd (Left err) = Left err
continue cmd (Right s)  = evalComm cmd s

evalComm :: Comm -> State -> Either Error State
evalComm Skip               s = Right s
evalComm (Let v i)          s = case (evalIntExp i s) of
        Left err -> Left err
        Right (i', s') -> update v i' s'
evalComm (Seq x y)          s = case (evalComm x s) of
        Left err -> Left err
        Right s' -> evalComm y s'
evalComm (IfThenElse b x y) s = case (evalBoolExp b s) of
        Left err -> Left err
        Right (True, s') -> evalComm x s'
        Right (False, s') -> evalComm y s'
evalComm (While b c)        s = case (evalBoolExp b s) of
        Left err -> Left err
        Right (True, s') -> continue (While b c) $ evalComm c s'
        Right (False, s') -> Right s'

-- Evalua una expresion entera, con efectos laterales
binop :: (Integer -> Integer -> b) -> IntExp -> IntExp -> State -> Either Error (b, State) 
binop f x y s = case (evalIntExp x s) of
        Left err       -> Left err
        Right (x', s') -> binop' f x' y s' 

binop' :: (Integer -> Integer -> b) -> Integer -> IntExp -> State -> Either Error (b, State) 
binop' f x y s = case (evalIntExp y s) of 
        Left err       -> Left err
        Right (y', s') -> Right (f x y', s')

binopDiv :: IntExp -> IntExp -> State -> Either Error (Integer, State) 
binopDiv x y s = case (evalIntExp x s) of
        Left err       -> Left err
        Right (x', s') -> binopDiv' x' y s' 

binopDiv' x y s = case (evalIntExp y s) of 
        Left err       -> Left err
        Right (0, s')  -> Left DivByZero
        Right (y', s') -> Right (div x y', s')


evalIntExp :: IntExp -> State -> Either Error (Integer, State)
evalIntExp (Const c)      s = Right (c, s)
evalIntExp (Var x)        s = case (lookfor x s) of
        Right x'  -> Right (x', s)
        otherwise -> Left UndefVar
evalIntExp (UMinus x)     s = binop (*) x (Const (-1)) s
evalIntExp (Plus  x y)    s = binop (+) x y s
evalIntExp (Minus x y)    s = binop (-) x y s
evalIntExp (Times x y)    s = binop (*) x y s
evalIntExp (Div   x y)    s = binopDiv x y s
evalIntExp (Assign v i)   s = case evalIntExp i s of
        Left err -> Left err
        Right (val, s1) -> case (update v val s1) of 
                Left err -> Left err
                Right s2 -> Right (val, s2)
evalIntExp (Secuence x y) s = case (evalIntExp x s) of
        Left err -> Left err
        Right (x', s') -> evalIntExp y s'
        
-- Evalua una expresion entera, sin efectos laterales
binopBool :: (Bool -> Bool -> b) -> BoolExp -> BoolExp -> State -> Either Error (b, State) 
binopBool f x y s = case (evalBoolExp x s) of 
        Left err -> Left err
        Right (x', s') -> binopBool' f x' y s'
        
binopBool' :: (Bool -> Bool -> b) -> Bool -> BoolExp -> State -> Either Error (b, State) 
binopBool' f x y s = case (evalBoolExp y s) of 
        Left err -> Left err
        Right (y', s') -> Right (f x y', s')

evalBoolExp :: BoolExp -> State -> Either Error (Bool, State)
evalBoolExp BTrue     s = Right (True, s)
evalBoolExp BFalse    s = Right (False, s)
evalBoolExp (Eq x y)  s = binop (==) x y s
evalBoolExp (NEq x y) s = binop (/=) x y s
evalBoolExp (Lt x y)  s = binop (<) x y s
evalBoolExp (Gt x y)  s = binop (>) x y s
evalBoolExp (And x y) s = binopBool (&&) x y s
evalBoolExp (Or x y)  s = binopBool (||) x y s
evalBoolExp (Not x)   s = case (evalBoolExp x s) of
        Left err -> Left err
        Right (b, s') -> Right (not b, s')

