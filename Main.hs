module Main (main) where

import qualified System as S
import Lexer
import Parser

main = do
       args <- S.getArgs
       nombreArchivo <- return $ head args
       contenido <- readFile nombreArchivo
       print $ parse $ lexer contenido
