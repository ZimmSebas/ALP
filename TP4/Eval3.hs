module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []


-- Evalua un programa en el estado nulo
-- eval :: [dar el tipo segun corresponda]
eval = undefined

-- Evalua un comando en un estado dado
-- evalComm :: [dar el tipo segun corresponda]
evalComm = undefined

-- Evalua una expresion entera en un estado dado
-- evalIntExp :: [dar el tipo segun corresponda]
evalIntExp = undefined

-- Evalua una expresion booleana en un estado dado
-- evalBoolExp :: [dar el tipo segun corresponda]
evalBoolExp = undefined
