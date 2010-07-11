module Main (main) where

import qualified System as S
import Lexer
import Parser
import TablaSimbolos
import TablaSimbolos
import GeneracionCodigo

main = do
       args <- S.getArgs
       nombreArchivo <- return $ head args
       contenido <- readFile nombreArchivo
       tabla <- nuevaTablaDeSimbolos
       arbol <- return $ parse (lexer contenido)
       print arbol
       procesar arbol tabla
