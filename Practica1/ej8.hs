import Parsing
import Control.Monad
import Control.Applicative

data Expr = N Int | Sum Expr Expr | Min Expr Expr | Prod Expr Expr | Div Expr Expr deriving Show

expr   :: Parser Expr
expr   = do { t <- term
            ; f <- expr'
            ; return (f t)}
            
expr' :: Parser (Expr -> Expr)
expr' = do  { char '+'
            ; v <- term
            ; f <- expr'
            ; return (f . (\e1 -> Sum e1 v))}
          <|> do { char '-'
                 ; v <- term
                 ; f <- expr'
                 ; return (f . (\e1 -> Min e1 v))}
          <|> return id

term  :: Parser Expr
term  = do { t <- factor
           ; f <- term'
           ; return (f t)}

term' :: Parser (Expr -> Expr)
term' = do { char '*'
           ; v <- factor
           ; f <- term'
           ; return ( f . (\t1 -> Prod t1 v))}
         <|> do { char '/'
                ; v <- factor
                ; f <- term'
                ; return (f . (\t1 -> Div t1 v))}
         <|> return id

factor :: Parser Expr
factor = do { char '('
            ; e <- expr
            ; char ')'
            ; return (e)}
         <|> do { e <- integer
                ; return (N e)}



-- ~ expr :: Parser Expr
-- ~ expr = do { t <-term
          -- ~ ;(do { char '+'
              -- ~ ; e <- expr
              -- ~ ;return (Add t e) }
            -- ~ <|> do { char '-'
              -- ~ ; e <- expr
              -- ~ ; return (Min t e)}
            -- ~ <|> return t)}

-- ~ term :: Parser Expr
-- ~ term = do { f <- factor
           -- ~ ;(do { char '*'
               -- ~ ; t <- term
               -- ~ ; return (Prod f t)} 
            -- ~ <|> do  { char '/'
               -- ~ ; t <- term
               -- ~ ; return (Div f t)}
            -- ~ <|> return f)}

