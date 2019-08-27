import Parsing
import Control.Monad
import Control.Applicative hiding (many)

data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Add | Mul | Min | Div deriving Show

expr :: Parser Expr
expr = do t <- term
          (do char '+'
              e <- expr
              return (BinOp Add t e))
            <|> do char '-'
                   e <- expr
                   return (BinOp Min t e)
            <|> return t

term :: Parser Expr
term = do f <- factor
          do char '*'
             t <- term
             return (BinOp Mul f t)
           <|> do char '/'
                  t <- term
                  return (BinOp Div f t)
           <|> return f

factor :: Parser Expr 
factor = do{ x <- int; return (Num x)}
         <|>
         do{char '('; e <- expr; char ')'; return e}

 
