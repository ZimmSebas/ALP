import Parsing
import Control.Monad
import Control.Applicative

trans :: Parser a -> Parser a
trans p = p <|> do {char '('
		   ;jorge <- p 
		   ;char ')'
		   ;return jorge} 
