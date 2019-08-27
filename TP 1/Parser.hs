module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","if","then",
                                                     "else", "while"] 
                                  , reservedOpNames = ["-", "+", "="]                   
                                  })
  
----------------------------------
--- Parser de expressiones enteras
-----------------------------------

intexp :: Parser IntExp
intexp  = try (do x<- identifier lis
                  return (Var x)) 
          <|> try (do { a <- intexp 
                       ; reservedOp lis "-"
                       ; b <- intexp
                       ; return (Minus a b)})
          <|> do { n <- natural lis ; return (Const n)}


            -- | Var Variable
            -- | UMinus IntExp
            -- | Plus IntExp IntExp
            -- | Minus IntExp IntExp
            -- | Times IntExp IntExp
            -- | Div IntExp IntExp
            -- | Assign Variable IntExp
            -- | Secuence IntExp IntExp
 -- deriving Show



-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser BoolExp
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)



-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec-Token.html#v:integer
