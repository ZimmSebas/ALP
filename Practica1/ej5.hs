import Parsing
import Control.Monad
import Control.Applicative

type Het = Either Int Char

parseHet :: Parser Het
parseHet = do { x <- int
	      ; return (Left x)}
	    <|> do { char '\''
		   ; x <- item
	           ; char '\'' 
	           ; return (Right x)}

parseHetList :: Parser [Het]
parseHetList = do {char '['
	          ;jorge <- sepBy parseHet (char ',')
		  ;char ']'
		  ;return jorge}
