module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)        = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)       = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)     = Lam t (conversion' (n:b) u)
conversion' b (LNum n)        = Num n
conversion' b (Succ t)        = Suc $ conversion' b t
conversion' b (Rec t1 t2 t3)  = R (conversion' b t1) (conversion' b t2) (conversion' b t3) 
conversion' b (LNil)          = Nill
conversion' b (Cons x xs)     = Con (conversion' b x) (conversion' b xs)
conversion' b (RecL t1 t2 t3) = RL (conversion' b t1) (conversion' b t2) (conversion' b t3)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub _ _ (Num n)               = Num n
sub i t (Suc n)               = Suc (sub i t n)
sub i t (R t1 t2 t3)          = R (sub i t t1) (sub i t t2) (sub i t t3)
sub _ _ (Nill)                = Nill
sub i t (Con x xs)            = Con (sub i t x) (sub i t xs)
sub i t (RL t1 t2 t3)         = RL (sub i t t1) (sub i t t2) (sub i t t3)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u :@: v)       = case eval e v of
                 VLam t' u' -> eval e (Lam t u :@: Lam t' u')
                 VNum n     -> eval e (sub 0 (Num n) u)
                 VL xs      -> case xs of 
                        VNil       -> eval e (sub 0 Nill u)
                        VCons x xs -> eval e (sub 0 (Con (quote x) (quote $ VL xs)) u)
                 _          -> error "Error de tipo en run-time, verificar type checker"
eval e (u :@: v)             = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Num n)               = VNum n
eval e (Suc t)               = case eval e t of 
                 VNum n    -> VNum (n + 1)
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (R t1 t2 (Num n))     = case n of
                 0         -> eval e t1
                 _         -> eval e (t2 :@: (R t1 t2 $ Num (n-1)) :@: (Num (n-1))) 
eval e (R t1 t2 t3)          = case eval e t3 of
                 VNum n    -> eval e (R t1 t2 $ Num n)
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Nill)                = VL VNil
eval e (Con x xs)            = VL $ (eval' e (Con x xs))
eval e (RL t1 t2 (Nill))     = eval e t1
eval e (RL t1 t2 (Con x xs)) = eval e (t2 :@: x :@: xs :@: (RL t1 t2 xs))
eval e (RL t1 t2 t3)         = case quote $ eval e t3 of
                 Nill      -> eval e t1
                 Con x xs  -> eval e (t2 :@: x :@: xs :@: (RL t1 t2 Nill))
                 _               -> error "Error de tipo en run-time, verificar type checker"

eval' e (Nill)               = VNil
eval' e (Con x xs)           = VCons (eval e x) (eval' e xs)
eval' e _                    = error "Error de tipo en run-time, verificar type checker"

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)        = Lam t f
quote (VNum n)          = Num n
quote (VL VNil)         = Nill
quote (VL (VCons x xs)) = Con (quote x) (quote $ VL xs)

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

rmatchError :: Type -> Type -> Either String Type
rmatchError t1 t2 = err $ "se esperaba " ++
                          render (printType (Fun t1 (Fun Nat t1))) ++
                          ", pero " ++
                          render (printType t2) ++
                          " fue inferido."

rlmatchError :: Type -> Type -> Either String Type
rlmatchError t1 t2 = err $ "se esperaba " ++
                           render (printType (Fun Nat (Fun ListNat (Fun t1 t1)))) ++
                           ", pero " ++
                           render (printType t2) ++
                           " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i)     = ret (c !! i)
infer' _ e (Free n)      = case lookup n e of
                             Nothing -> notfoundError n
                             Just (_,t) -> ret t
infer' c e (t :@: u)     = infer' c e t >>= \tt -> 
                           infer' c e u >>= \tu ->
                           case tt of
                             Fun t1 t2 -> if (tu == t1) 
                                         then ret t2
                                         else matchError t1 tu
                             _         -> notfunError tt
infer' c e (Lam t u)     = infer' (t:c) e u >>= \tu ->
                           ret $ Fun t tu
infer' c e (Num n)       = Right Nat
infer' c e (Suc t)       = infer' c e t >>= \tt ->
                           case tt of
                             Nat -> ret Nat
                             t1  -> matchError Nat t1
infer' c e (Nill)        = Right ListNat
infer' c e (Con t1 t2)   = infer' c e t1 >>= \tt1 ->
                           infer' c e t2 >>= \tt2 ->
                           case tt1 of 
                             Nat -> if (tt2 == ListNat)
                                    then ret tt2
                                    else matchError ListNat tt2
                             _   -> matchError Nat tt1
infer' c e (R t1 t2 t3)  = infer' c e t1 >>= \tt1 -> 
                           infer' c e t2 >>= \tt2 ->
                           infer' c e t3 >>= \tt3 ->
                           case tt2 of
                             Fun t (Fun Nat t') -> 
                               if (tt1 /= t || tt1 /= t')
                               then rmatchError tt1 tt2
                               else if (tt3 /= Nat) 
                                 then matchError Nat tt3
                                 else ret t
                             Fun _ _            -> rmatchError tt1 tt2
                             _                  -> notfunError tt2
infer' c e (RL t1 t2 t3) = infer' c e t1 >>= \tt1 ->
                           infer' c e t2 >>= \tt2 -> 
                           infer' c e t3 >>= \tt3 ->
                           case tt2 of 
                           Fun Nat (Fun ListNat (Fun t t')) -> 
                             if (tt1 /= t || tt1 /= t')
                             then rlmatchError tt1 tt2
                             else if (tt3 /= ListNat)
                                  then matchError ListNat tt3
                                  else ret t
                           Fun _ _  -> rlmatchError tt1 tt2
                           _        -> notfunError tt2









