import Parsing
import Control.Monad
import Control.Applicative

data Hasktype = DInt | DChar | Fun Hasktype Hasktype deriving Show

tipo :: Parser Hasktype
tipo =  do{ x <- (token (string "Int"))
          ; (do { string "->"
                 ; t <- tipo
                 ; return (Fun DInt t)})
            <|> return DInt}
        <|> do {x <- (token (string "Char"))
                ;(do {string "->"
                    ; t <- tipo
                    ; return (Fun DChar t)})
                <|> return DChar}
