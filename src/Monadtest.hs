module Monadtest where
import Main

calc :: Either String Double -> Either String Double -> Either String Double 
calc a b = do 
    x <- a
    y <- b
    return (x + y) 

-- >>= ::®