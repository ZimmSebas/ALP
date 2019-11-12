module Eval1 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]
 
-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }
-- State (Env -> (a, Env))

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f  = State (\s -> let (v, s') = runState m s in
                            runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM
 
instance Applicative State where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)


-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
evalComm Skip                 = return ()
evalComm (Let v e)            = do m <- evalIntExp e 
                                   update v m
evalComm (Seq c1 c2)          = do evalComm c1
                                   evalComm c2
evalComm (IfThenElse e c1 c2) = do b <- evalBoolExp e
                                   if b then evalComm c1
                                        else evalComm c2
evalComm (While e c1)         = do b <- evalBoolExp e
                                   if b then (do { evalComm c1 ; evalComm (While e c1) })
                                        else (return ())

-- Evalua una expresion entera en un estado dado
evalIntExp :: MonadState m => IntExp -> m Int
evalIntExp (Const n)     = return n
evalIntExp (Var x)       = lookfor x
evalIntExp (UMinus e)    = evalIntExp e >>= return . (*(-1))
evalIntExp (Plus x y)    = do n <- evalIntExp x
                              m <- evalIntExp y
                              return (n + m)
evalIntExp (Minus x y)   = do n <- evalIntExp x
                              m <- evalIntExp y
                              return (n - m)
evalIntExp (Times x y)   = do n <- evalIntExp x
                              m <- evalIntExp y
                              return (n * m)
evalIntExp (Div x y)     = do n <- evalIntExp x
                              m <- evalIntExp y
                              return (n `div` m)
evalIntExp (Ass v e)     = do m <- evalIntExp e 
                              update v m
                              return m
evalIntExp (SeqIE e1 e2) = do evalIntExp e1
                              evalIntExp e2                       

-- Evalua una expresion booleana en un estado dado
evalBoolExp :: MonadState m => BoolExp -> m Bool
evalBoolExp BTrue        = return True
evalBoolExp BFalse       = return False
evalBoolExp (Eq p q)     = do x <- evalIntExp p
                              y <- evalIntExp q
                              return (x == y)
evalBoolExp (NEq p q) = do x <- evalIntExp p
                           y <- evalIntExp q
                           return (x /= y)
evalBoolExp (Lt p q)  = do x <- evalIntExp p
                           y <- evalIntExp q
                           return (x < y)
evalBoolExp (Gt p q)  = do x <- evalIntExp p
                           y <- evalIntExp q
                           return (x > y)
evalBoolExp (And p q) = do x <- evalBoolExp p
                           y <- evalBoolExp q
                           return (x && y)
evalBoolExp (Or p q)  = do x <- evalBoolExp p
                           y <- evalBoolExp q
                           return (x || y)
evalBoolExp (Not p)   = do x <- evalBoolExp p
                           return (not x)
                                          
