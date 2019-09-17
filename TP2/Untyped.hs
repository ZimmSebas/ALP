module Untyped where

import Control.Monad
import Data.List

import Common


------------------------------------------------
-- Sección 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
------------------------------------------------

conversion  :: LamTerm -> Term
conversion t = conversion' t []

conversion' :: LamTerm -> [String] ->Term
conversion' (LVar s)    state = case (elemIndex s state) of
            Just n -> Bound (length state - n - 1)
            _      -> Free s
conversion' (App t1 t2) state = let v1 = conversion' t1 state
                                    v2 = conversion' t2 state
                                in (v1 :@: v2)
conversion' (Abs str t) state = let s = str : state 
                                in (Lam $ conversion' t s)


  
-------------------------------
-- Sección 3 - Evaluación
-------------------------------

shift :: Term -> Int -> Int -> Term
shift = undefined
  
  
subst :: Term -> Term -> Int -> Term
subst = undefined


eval :: NameEnv Term -> Term -> Term
eval = undefined
    
    
    
    
    
