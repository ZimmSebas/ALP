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
shift (Bound k) c d | k >= c    = Bound $ k+d
                    | otherwise = Bound k
shift (Free x)  c d = Free x
shift (t :@: u) c d = (shift t c d) :@: (shift u c d)
shift (Lam t)   c d = Lam (shift t c d)

subst :: Term -> Term -> Int -> Term
subst t1 t2 i = subst' t1 t2 i 0

subst' :: Term -> Term -> Int -> Int -> Term
subst' t1 t2 i j = case t1 of
    (Bound k) -> substBound k t2 i j
    (v :@: u) -> (subst' v t2 i j) :@: (subst' u t2 i j)
    (Lam t)   -> Lam (subst' t t2 i (j+1))

substBound :: Int -> Term -> Int -> Int -> Term
substBound k t2 i j | k == i = shift t2 k j
                    | k >  i = Bound (k-1)
                    | k <  i = Bound k

eval :: NameEnv Term -> Term -> Term
eval = undefined
    
    
    
    
    
