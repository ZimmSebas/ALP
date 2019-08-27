import Parsing
import Control.Monad
import Control.Applicative hiding (many)



expr :: Parser Int
expr = do t <- term
          (do char '+'
              e <- expr
              return (t+e))
            <|> do char '-'
                   e <- expr
                   return (t-e)
            <|> return t

term :: Parser Int
term = do f <- factor
          do char '*'
             t <- term
             return (f*t)
           <|> do char '/'
                  t <- term
                  return (f `div` t)
           <|> return f

factor :: Parser Int
factor = int <|> do char '('
                    e <- expr
                    char ')'
                    return e

 
