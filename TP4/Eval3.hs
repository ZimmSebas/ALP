module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]
-- Costo
type Cost = Int

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado, con manejo de errores y costo
newtype StateErrorCost a = StateErrorCost { runStateErrorCost :: Env -> Maybe (a, Env, Cost) }

-- Para calmar al GHC
instance Functor StateErrorCost where
    fmap = liftM
 
instance Applicative StateErrorCost where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()
    
-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

class Monad m => MonadCost m where
    -- Aumenta el costo en uno
    tick :: m ()  

instance Monad StateErrorCost where
    return x = StateErrorCost (\s -> Just (x, s, 0))
    m >>= f  = StateErrorCost (\s -> case (runStateErrorCost m s) of
                                        Nothing -> Nothing
                                        Just (v, s', m) -> case (runStateErrorCost (f v) s') of
                                            Nothing         -> Nothing
                                            Just (u, st, n) -> Just (u, st, m+n))

instance MonadError StateErrorCost where
    throw = StateErrorCost (\s -> Nothing)

instance MonadState StateErrorCost where
    lookfor v = StateErrorCost (\s -> case (lookfor' v s) of
                                        Nothing -> Nothing
                                        Just j  -> Just (j, s, 0))
                    where lookfor' v []          = Nothing
                          lookfor' v ((u, j):ss) | v == u = Just j
                                                 | v /= u = lookfor' v ss
    update v i = StateErrorCost (\s -> Just ((), update' v i s, 0))
                     where update' v i [] = [(v, i)]
                           update' v i ((u, _):ss) | v == u = (v, i):ss
                           update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

instance MonadCost StateErrorCost where
    tick = StateErrorCost (\s -> Just ((), s, 1))

-- Evalua un programa en el estado nulo
eval :: Comm -> Maybe (Env, Cost)
eval p = case (runStateErrorCost (evalComm p) initState) of
    Nothing         -> Nothing
    Just (_, s, c)  -> Just (s, c)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadCost m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m, MonadCost m) => IntExp -> m Int
evalIntExp (Const n)     = return n
evalIntExp (Var v)       = lookfor v
evalIntExp (UMinus e)    = do n <- evalIntExp e 
                              tick
                              return (-n)
evalIntExp (Plus x y)    = do n <- evalIntExp x
                              m <- evalIntExp y
                              tick
                              return (n + m)
evalIntExp (Minus x y)   = do n <- evalIntExp x
                              m <- evalIntExp y
                              tick
                              return (n - m)
evalIntExp (Times x y)   = do n <- evalIntExp x
                              m <- evalIntExp y
                              tick
                              tick
                              return (n * m)
evalIntExp (Div x y)     = do n <- evalIntExp x
                              m <- evalIntExp y
                              tick
                              tick
                              if m == 0 then throw
                                        else return (n `div` m) 
                              return (n `div` m)
evalIntExp (Ass v e)     = do m <- evalIntExp e 
                              update v m
                              return m
evalIntExp (SeqIE e1 e2) = do evalIntExp e1
                              evalIntExp e2                     

-- Evalua una expresion booleana en un estado dado
evalBoolExp :: (MonadState m, MonadError m, MonadCost m) => BoolExp -> m Bool
evalBoolExp BTrue     = return True
evalBoolExp BFalse    = return False
evalBoolExp (Eq p q)  = do x <- evalIntExp p
                           y <- evalIntExp q
                           tick
                           return (x == y)
evalBoolExp (NEq p q) = do x <- evalIntExp p
                           y <- evalIntExp q
                           tick
                           return (x /= y)
evalBoolExp (Lt p q)  = do x <- evalIntExp p
                           y <- evalIntExp q
                           tick
                           return (x < y)
evalBoolExp (Gt p q)  = do x <- evalIntExp p
                           y <- evalIntExp q
                           tick
                           return (x > y)
evalBoolExp (And p q) = do x <- evalBoolExp p
                           y <- evalBoolExp q
                           tick
                           return (x && y)
evalBoolExp (Or p q)  = do x <- evalBoolExp p
                           y <- evalBoolExp q
                           tick
                           return (x || y)
evalBoolExp (Not p)   = do x <- evalBoolExp p
                           tick
                           return (not x)

