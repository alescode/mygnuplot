module Main (main) where

import qualified System as S
import Lexer
import Parser
import TablaSimbolos
import TablaSimbolos
import GeneracionCodigo
import System.IO

main = do
       args <- S.getArgs
       nombreEntrada <- return $ head args
       nombreSalida <- return $ (head args) ++ ".pl"

       aSalida <- openFile nombreSalida WriteMode

       contenido <- readFile nombreEntrada
       tabla <- nuevaTablaDeSimbolos
       arbol <- return $ parse (lexer contenido)
       print arbol

       generarCodigo arbol tabla aSalida
       a <- toList tabla
       print a  
       hClose aSalida
