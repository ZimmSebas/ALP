import Parsing
import Control.Monad
import Control.Applicative

clparse = do { symbol "["
             ; xs <- sepBy int (symbol ",")
             ; symbol "]"
             ; return xs}
            <|> do { n <- int
                   ; symbol ":"
                   ; xs <- clparse
                   ; return ([n] ++ xs)}


-- clparse = do { n <- int;
               -- symbol ":";
               -- xs <- clparse;
               -- return [n] ++ xs
               


parselist = do { xs <- clparse;
                 (do { symbol "++"
                     ; ys <- parselist
                     ; return (xs ++ ys)}
                  <|> return xs)}
