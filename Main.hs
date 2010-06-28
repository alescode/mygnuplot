module Main (main) where

import System
import System.IO 
import Lexer
import Parser

main =
	do
		args <- getArgs
		nombreArchivo <- return $ head args
		contenido <- readFile nombreArchivo
		print $ parse $ lexer contenido
