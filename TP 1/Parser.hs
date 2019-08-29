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
                                                     "else", "while", "skip"] 
                                  , reservedOpNames = ["-", "+", "=","*", "/", "==", 
                                                       "!=", "&&", "||", "<", ">", "!", ";"]                   
                                  })
  
----------------------------------
--- Common Parsers
-----------------------------------
parenParse  :: Parser p -> Parser p
parenParse p = do { symbol lis "("
                  ; x <- p
                  ; symbol lis ")"
                  ; return x }
                  
----------------------------------
--- Parser de expressiones enteras
-----------------------------------                  
opParseTerm = do  { reservedOp lis "+" ; return (Plus) }
              <|> do { reservedOp lis "-" ; return (Minus) }
           
opParseFactor = do  { reservedOp lis "*" ; return (Times) }
                <|> do { reservedOp lis "/" ; return (Div) }
 
negativeParse :: Parser IntExp
negativeParse = do { reservedOp lis "-" 
                  ; b <- factorParse
                  ; return (UMinus b) }
                  
constParse :: Parser IntExp
constParse  = do { f <- natural lis
                 ; return (Const f) }

varParse :: Parser IntExp
varParse  = do x<- identifier lis
               return (Var x)

intexp :: Parser IntExp
intexp  = chainl1 termParse opParseTerm
          
termParse :: Parser IntExp
termParse  = chainl1 factorParse opParseFactor

factorParse :: Parser IntExp
factorParse  = negativeParse
               <|> try constParse
               <|> try varParse 
               <|> parenParse intexp



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
                    ; b <- bTermParse  -- ACA FALTA UN EQUIVALENTE A FACTOR
                    ; return (Not b) }

comparisonParse :: Parser BoolExp
comparisonParse  = do { x <- intexp
                      ; f <- compOpParse
                      ; y <- intexp
                      ; return (f x y)}


boolexp :: Parser BoolExp
boolexp  = chainl1 bTermParse boolOpParse

bTermParse :: Parser BoolExp
bTermParse  = negationParse
              <|> try boolPrimitive
              <|> parenParse boolexp
              <|> comparisonParse

-----------------------------------
--- Parser de comandos
-----------------------------------

dacParse  = do  { reservedOp lis ";" ; return (Seq) }

commLine :: Parser Comm
commLine  = do { reserved lis "skip" 
               ; return (Skip)}


comm :: Parser Comm
comm = chainl1 commLine dacParse

-- ~ data Comm = Skip
          -- ~ | Let Variable IntExp
          -- ~ | Seq Comm Comm
          -- ~ | IfThenElse BoolExp Comm Comm
          -- ~ | While BoolExp Comm 
 -- ~ deriving Show


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)



-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec-Token.html#v:integer
