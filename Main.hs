module Main (main) where

import qualified System as S
import Lexer
import Parser
import TablaSimbolos
import GeneracionCodigo
import System.IO
import Data.Map as Map

main = do
       args <- S.getArgs
       nombreEntrada <- return $ head args
       nombreSalida <- return $ (head args) ++ ".pl"

       aSalida <- openFile nombreSalida WriteMode

       contenido <- readFile nombreEntrada
       arbol <- return $ parse (lexer contenido)
       print arbol

       generarCodigo arbol Map.empty aSalida
       hClose aSalida
