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
                                  , reservedOpNames = ["-", "+", "=","*", "/", "==", 
                                                       "!=", "&&", "||", "<", ">", "!"]                   
                                  })
  
----------------------------------
--- Parser de expressiones enteras
-----------------------------------

opParseTerm = do  { reservedOp lis "+" ; return (Plus) }
              <|> do { reservedOp lis "-" ; return (Minus) }
           
opParseFactor = do  { reservedOp lis "*" ; return (Times) }
                <|> do { reservedOp lis "/" ; return (Div) }
 
negativParse :: Parser IntExp
negativParse = do { reservedOp lis "-" 
                  ; b <- factorParse
                  ; return (UMinus b) }
                  
constParse :: Parser IntExp
constParse  = do { f <- natural lis
                 ; return (Const f) }

varParse :: Parser IntExp
varParse  = do x<- identifier lis
               return (Var x)

parenParse :: Parser IntExp
parenParse  = do { symbol lis "("
                 ; x <- intexp
                 ; symbol lis ")"
                 ; return x }

intexp :: Parser IntExp
intexp  = chainl1 termParse opParseTerm
          
termParse :: Parser IntExp
termParse  = chainl1 factorParse opParseFactor

factorParse :: Parser IntExp
factorParse  = negativParse
              <|> try constParse
               <|> try varParse 
               <|> parenParse



-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolPrimitive :: Parser BoolExp
boolPrimitive  = do { f <- reserved lis "true"; return (BTrue) }
                 <|> do { f <- reserved lis "false"; return (BFalse) }

compOpParse = do  { reservedOp lis "==" ; return (Eq) }
              <|> do { reservedOp lis "!=" ; return (NEq) }
              <|> do { reservedOp lis "<" ; return (Lt) }
              <|> do { reservedOp lis ">" ; return (Gt) }

boolOpParse = do  { reservedOp lis "&&" ; return (And) }
              <|> do { reservedOp lis "||" ; return (Or) }
              
negationParse :: Parser BoolExp
negationParse  = do { reservedOp lis "!" 
                    ; b <- boolexp  -- ACA FALTA UN EQUIVALENTE A FACTOR
                    ; return (Not b) }


boolexp :: Parser BoolExp
boolexp = undefined



-- ~ data BoolExp = BTrue
             -- ~ | BFalse
             -- ~ | Eq IntExp IntExp
             -- ~ | NEq IntExp IntExp
             -- ~ | Lt IntExp IntExp
             -- ~ | Gt IntExp IntExp
             -- ~ | And BoolExp BoolExp
             -- ~ | Or BoolExp BoolExp
             -- ~ | Not BoolExp
 -- ~ deriving Show


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
