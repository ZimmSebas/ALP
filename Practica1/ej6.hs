import Parsing
import Control.Monad
import Control.Applicative

data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype]

aux :: Parser Basetype
aux = do{ string "Int"; return DInt} <|>
      do{ string "Char"; return DChar} <|>
      do{ string "Float"; return DFloat}

tipo :: Parser Hasktype
tipo = sepBy1 (token aux) (string "->")   
