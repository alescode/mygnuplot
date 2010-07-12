module TablaSimbolos (
TablaDeSimbolos,
funcionesPredefinidas,
Simbolo) where

import Data.Map
import Data.Int
import Data.Char
import AS

--stringHash :: String -> Int32
--stringHash str = stringHash' str 1
--
---- fold?
--stringHash' [] _         = 0
--stringHash' (char:str) n = (toEnum $ ord char)*n + stringHash' str 2*n

type Simbolo = (String, EM) -- nombre de la variable, EM

type TablaDeSimbolos = Map String Simbolo -- key: nombre de la funcion

funcionesPredefinidas :: [String]
funcionesPredefinidas = ["sin", "cos", "tan", "exp", "log", "ceil", "floor"]
