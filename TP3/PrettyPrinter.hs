module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Prelude hiding ((<>))
import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [(1::Integer)..], c <- ['x','y','z'] ++ ['a'..'w'] ]
              
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  _  (Free (Global s)) = text s

pp ii vs (i :@: c) = sep [parensIf (isAny i) (pp ii vs i), 
                          nest 1 (parensIf (isLam c || isApp c || isRec c || isSuc c) (pp ii vs c))]  
pp ii vs (Lam t c) = text "\\" <>
                     text (vs !! ii) <>
                     text ":" <>
                     printType t <>
                     text ". " <> 
                     pp (ii+1) vs c
pp ii vs (Suc t)         = text "suc " <> parensIf (isAny t) (pp ii vs t)
pp ii vs (R t1 t2 t3)    = sep [text "rec", 
                             parensIf (isAny t1) (pp ii vs t1),
                             parensIf (isAny t2) (pp ii vs t2),
                             parensIf (isAny t3) (pp ii vs t3)]
pp ii vs (Num n)         = text $ show n
pp ii vs (Nill)          = text "[]"
pp ii vs (Con x xs)      = text "[" <>
                           pp' ii vs (Con x xs) <>
                           text "]"
pp ii vs (RL t1 t2 t3)   = sep [text "recl",
                             parensIf (isAny t1) (pp ii vs t1),
                             parensIf (isAny t2) (pp ii vs t2),
                             parensIf (isAny t3) (pp ii vs t3)]

pp' ii vs (Nill)         = text ""
pp' ii vs (Con x Nill)   = text $ show x
pp' ii vs (Con x xs)     = text (show x) <> text "," <> (pp' ii vs xs)

isAny :: Term -> Bool
isAny t = isLam t || isApp t || isRec t || isSuc t 

isLam :: Term -> Bool                    
isLam (Lam _ _) = True
isLam  _        = False

isApp :: Term -> Bool        
isApp (_ :@: _) = True
isApp _         = False

isSuc (Suc _)   = True
isSuc _         = False

isRec (R _ _ _) = True
isRec _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType Nat          = text "Nat"
printType ListNat      = text "[Nat]"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1), 
                               text "->", 
                               printType t2]
isFun :: Type -> Bool
isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Num _)           = [] 
fv (Free (Global n)) = [n]
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Suc t)           = fv t
fv (Nill)            = []
fv (Con _ _)         = []
fv (R t1 t2 t3)      = fv t1 ++ fv t2 ++ fv t3
fv (RL t1 t2 t3)     = fv t1 ++ fv t2 ++ fv t3

---
printTerm :: Term -> Doc 
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

