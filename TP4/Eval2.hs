module Eval2 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado, con manejo de errores
newtype StateError a = StateError { runStateError :: Env -> Maybe (a, Env) }

-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM
 
instance Applicative StateError where
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

instance Monad StateError where
    return x = StateError (\s -> Just (x, s))
    m >>= f  = StateError (\s -> case (runStateError m s) of
                                     Nothing -> Nothing
                                     Just (v, s') -> runStateError (f v) s')

instance MonadError StateError where
    throw = StateError (\s -> Nothing)

instance MonadState StateError where
    lookfor v = StateError (\s -> case (lookfor' v s) of
                                      Nothing -> Nothing
                                      Just j  -> Just (j, s))
                where lookfor' v []          = Nothing
                      lookfor' v ((u, j):ss) | v == u = Just j
                                             | v /= u = lookfor' v ss
    update v i = StateError (\s -> Just ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Evalua un programa en el estado nulo
eval :: Comm -> Maybe Env
eval p = case (runStateError (evalComm p) initState) of
    Nothing -> Nothing
    Just e  -> Just $ snd e

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
evalIntExp (Const n)     = return n
evalIntExp (Var v)       = lookfor v
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
                              if m == 0 then throw
                                        else return (n `div` m) 
                              return (n `div` m)
evalIntExp (Ass v e)     = do m <- evalIntExp e 
                              update v m
                              return m
evalIntExp (SeqIE e1 e2) = do evalIntExp e1
                              evalIntExp e2                       
 
-- Evalua una expresion booleana en un estado dado
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp BTrue     = return True
evalBoolExp BFalse    = return False
evalBoolExp (Eq p q)  = do x <- evalIntExp p
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
