module Eval3 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Trabajo
type Work = Integer

-- Error
data Error = DivByZero | UndefVar deriving Show

-- Estado nulo
initState :: State
initState = [("t",0),("n",0),("i",0),("a",0)]

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Either Error Integer
lookfor var state = let l = filter (\(v,i) -> v==var) state
                    in if (length l == 0) then Left UndefVar
                                          else Right $ snd $ head l

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> Either Error State
update var int state = case (lookfor var state) of
        Right _   -> Right $ map (\(v,i) -> if (v == var) then (var, int) else (v,i)) state
        otherwise -> Left UndefVar

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error (Work, State)
eval p = evalComm p (0, initState)

-- Evalua un comando en un estado dado
-- Completar definicion
continue :: Comm -> Either Error (Work, State) -> Either Error (Work, State)
continue cmd (Left err)     = Left err
continue cmd (Right (w,s))  = evalComm cmd (w, s)

evalComm :: Comm -> (Work, State) -> Either Error (Work, State)
evalComm Skip              (w, s) = Right (w, s)
evalComm (Let v i)         (w, s) = case (evalIntExp i s) of
        Left err             -> Left err
        Right (w', (i', s')) -> let ret = update v i' s'
                                in calcwork (w+w') ret
                where calcwork _ (Left err) = Left err
                      calcwork w (Right s)  = Right (w, s)
evalComm (Seq x y)         (w, s) = case (evalComm x (w, s)) of
        Left err       -> Left err
        Right (w', s') -> evalComm y (w', s')
evalComm (IfThenElse b x y) (w, s) = case (evalBoolExp b s) of
        Left err                -> Left err
        Right (w', (True, s'))  -> evalComm x (w+w', s')
        Right (w', (False, s')) -> evalComm y (w+w', s')
evalComm (While b c)        (w, s) = case (evalBoolExp b s) of
        Left err                -> Left err
        Right (w', (True, s'))  -> continue (While b c) $ evalComm c (w+w', s')
        Right (w', (False, s')) -> Right (w+w', s')

-- Evalua una expresion entera, con efectos laterales
-- Completar definicion

binop :: (Integer -> Integer -> b) -> IntExp -> IntExp -> State -> Either Error (Work, (b, State))
binop f x y s = case (evalIntExp x s) of
        Left err       -> Left err
        Right (w1, (x', s')) -> binop' w1 f x' y s' 

binop' :: Work -> (Integer -> Integer -> b) -> Integer -> IntExp -> State -> Either Error (Work, (b, State)) 
binop' w1 f x y s = case (evalIntExp y s) of 
        Left err       -> Left err
        Right (w2, (y', s')) -> Right (w1+w2+1, (f x y', s'))

binopMul :: IntExp -> IntExp -> State -> Either Error (Work, (Integer, State))
binopMul x y s = case (evalIntExp x s) of
        Left err       -> Left err
        Right (w1, (x', s')) -> binopMul' w1 x' y s' 

binopMul' :: Work -> Integer -> IntExp -> State -> Either Error (Work, (Integer, State)) 
binopMul' w1 x y s = case (evalIntExp y s) of 
        Left err       -> Left err
        Right (w2, (y', s')) -> Right (w1+w2+2, ( x * y', s'))

binopDiv :: IntExp -> IntExp -> State -> Either Error (Work, (Integer, State))
binopDiv x y s = case (evalIntExp x s) of
        Left err       -> Left err
        Right (w1, (x', s')) -> binopDiv' w1 x' y s' 

binopDiv' w1 x y s = case (evalIntExp y s) of 
        Left err       -> Left err
        Right (_, (0, s'))  -> Left DivByZero
        Right (w2, (y', s')) -> Right (w1+w2+2, (div x y', s'))

evalIntExp :: IntExp -> State -> Either Error (Work, (Integer, State))
evalIntExp (Const c)      s = Right (0, (c, s))
evalIntExp (Var x)        s = case (lookfor x s) of
        Right x'  -> Right (0, (x', s))
        otherwise -> Left UndefVar
evalIntExp (UMinus x)     s = binop (*) x (Const (-1)) s
evalIntExp (Plus  x y)    s = binop (+) x y s
evalIntExp (Minus x y)    s = binop (-) x y s
evalIntExp (Times x y)    s = binopMul x y s
evalIntExp (Div   x y)    s = binopDiv x y s
evalIntExp (Assign v i)   s = case evalIntExp i s of
        Left err -> Left err
        Right (w, (val, s1)) -> case (update v val s1) of 
                Left err -> Left err
                Right s2 -> Right (w, (val, s2))
evalIntExp (Secuence x y) s = case (evalIntExp x s) of
        Left err -> Left err
        Right (w, (x', s')) -> let ret = evalIntExp y s'
                               in calcwork w ret
                where calcwork w1 (Left err)             = Left err
                      calcwork w1 (Right (w2, (x2, s2))) = Right (w1+w2,(x2, s2))
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion

binopBool :: (Bool -> Bool -> b) -> BoolExp -> BoolExp -> State -> Either Error (Work, (b, State))
binopBool f x y s = case (evalBoolExp x s) of 
        Left err -> Left err
        Right (w1, (x', s')) -> binopBool' w1 f x' y s'
        
binopBool' :: Work -> (Bool -> Bool -> b) -> Bool -> BoolExp -> State -> Either Error (Work, (b, State)) 
binopBool' w1 f x y s = case (evalBoolExp y s) of 
        Left err -> Left err
        Right (w2, (y', s')) -> Right (w1+w2+1, (f x y', s'))

evalBoolExp :: BoolExp -> State -> Either Error (Work, (Bool, State))
evalBoolExp BTrue     s = Right (0, (True, s))
evalBoolExp BFalse    s = Right (0, (False, s))
evalBoolExp (Eq x y)  s = binop (==) x y s
evalBoolExp (NEq x y) s = binop (/=) x y s
evalBoolExp (Lt x y)  s = binop (<) x y s
evalBoolExp (Gt x y)  s = binop (>) x y s
evalBoolExp (And x y) s = binopBool (&&) x y s
evalBoolExp (Or x y)  s = binopBool (||) x y s
evalBoolExp (Not x)   s = case (evalBoolExp x s) of
        Left err -> Left err
        Right (w, (b, s')) -> Right (w+1, (not b, s'))


