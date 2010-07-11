module TablaSimbolos (module Data.HashTable,
nuevaTablaDeSimbolos,
TablaDeSimbolos,
Simbolo) where

import Data.HashTable
import Data.Int
import Data.Char
import AS

--stringHash :: String -> Int32
--stringHash str = stringHash' str 1
--
---- fold?
--stringHash' [] _         = 0
--stringHash' (char:str) n = (toEnum $ ord char)*n + stringHash' str 2*n

type Simbolo = EM

type TablaDeSimbolos = HashTable String Simbolo

nuevaTablaDeSimbolos :: IO (TablaDeSimbolos)
nuevaTablaDeSimbolos = new (==) hashString
