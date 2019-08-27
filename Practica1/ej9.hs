import Parsing
import Control.Monad
import Control.Applicative

data Ctype = Cons Type_Specifier Cdec deriving Show
data Cdec = Pun Cdec | Arr Cdec Int | Tup Cdec | I Identifier deriving Show
data Type_Specifier    = INT | CHAR | FLOAT deriving Show
type Identifier         = String
type Constant_Expresion = Int

typePars :: Parser Type_Specifier
typePars = do { x <- (token (string "int"))
              ; return INT}
           <|> do { x <- (token (string "char"))
                  ; return CHAR}
           <|> do { x <- (token (string "float"))
                  ; return FLOAT}

declaration :: Parser Ctype
declaration = do { tip <- typePars 
                 ; dec <- declarator
                 ; char ';'
                 ; return (Cons tip dec)}

declarator :: Parser Cdec
declarator =  do { char '*'
                 ; dec <- declarator
                 ; return (Pun dec)}
               <|> do { dir <- direct
                      ; return dir}

direct :: Parser Cdec
direct  = do d1 <- do symbol "("
                      dir <- direct
                      symbol ")"
                      return (Tup dir)
                     <|> do id <- identifier
                            return (I id)
             dd <- token direct'
             return (dd d1)
             
direct' :: Parser (Cdec -> Cdec)
direct'  = do { symbol "["
              ; tam <- integer
              ; symbol "]"
              ; dd <- direct'
              ; return (\x -> dd (Arr x tam))}
            <|> return id

