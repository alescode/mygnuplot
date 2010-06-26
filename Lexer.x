{
module Lexer (
    Token(..),
    lexer
) where
import qualified System.IO.UTF8 as U
}

%wrapper "posn"

$digito = 0-9           -- dígitos
$alfa = [a-zA-Z]        -- caracteres alfabéticos
$mm = [\+\-]            -- un símbolo más, o menos

tokens :-
    $white+                                          ;
    "#".*                                            ;
    \(                                               { obtenerToken $ const TkParentesisI }
    \)                                               { obtenerToken $ const TkParentesisD }
    $digito+                                         { obtenerToken TkEntero }
    $digito+ ("." $digito+)? ("e" $mm? $digito+)?    { obtenerToken TkReal }
    ("." $digito+) ("e" $mm? $digito+)?              { obtenerToken TkReal }
    \+                                               { obtenerToken $ const TkMas }
    \-                                               { obtenerToken $ const TkMenos } 
    \*                                               { obtenerToken $ const TkPor } 
    \/                                               { obtenerToken $ const TkEntre } 
    \^                                               { obtenerToken $ const TkElevado } 
    "pi" | "e"                                       { obtenerToken TkConstanteMat }
    \[                                               { obtenerToken $ const TkCorcheteI }
    \]                                               { obtenerToken $ const TkCorcheteD }
    "range"                                          { obtenerToken $ const TkRango }
    "if"                                             { obtenerToken $ const TkIf }
    "AND"                                            { obtenerToken $ const TkAnd }
    "OR"                                             { obtenerToken $ const TkOr }
    "NOT"                                            { obtenerToken $ const TkNot }
    "<"                                              { obtenerToken $ const TkMenor }
    "<="                                             { obtenerToken $ const TkMenorIg }
    ">"                                              { obtenerToken $ const TkMayor }
    ">="                                             { obtenerToken $ const TkMayorIg }
    "=="                                             { obtenerToken $ const TkIgual }
    "lines" "points"? | "points"                     { obtenerToken TkEstilo }
    "plot"                                           { obtenerToken $ const TkPlot }
    \{                                               { obtenerToken $ const TkLlaveI }
    \}                                               { obtenerToken $ const TkLlaveD }
    "with"                                           { obtenerToken $ const TkWith }
    "push_back"                                      { obtenerToken $ const TkPushBack }
    "for"                                            { obtenerToken $ const TkFor }
    "in"                                             { obtenerToken $ const TkIn }
    "step"                                           { obtenerToken $ const TkStep }
    "endfor"                                         { obtenerToken $ const TkEndfor }
    \'                                               { obtenerToken $ const TkComilla }
    \;                                               { obtenerToken $ const TkPuntoYComa }
    \,                                               { obtenerToken $ const TkComa }
    \=                                               { obtenerToken $ const TkAsignacion }
    "sin" | "cos" | "tan" | "exp" | "log" |
    "ceil" | "floor"                                 { obtenerToken TkFuncion }
    $alfa+                                           { obtenerToken TkIdentificador }

{
-- Todas las partes derechas tienen tipo (String -> Token),
-- especifica cuál es la función para convertir una cadena de
-- caracteres en un Token

-- La función obtenerToken hace que Alex devuelva tuplas con la linea
-- actual de lectura del analizador y el token leído
-- que se obtiene al aplicar la función f sobre el string leído
--obtenerToken :: (String -> Token) -> AlexPosn -> String -> (Int, Token)
--obtenerToken f pos str = (getPosnLine pos, f str)

-- La función obtenerToken hace que Alex devuelva tuplas con el estado
-- actual de lectura del analizador (AlexPosn) y el token leído
-- que se obtiene al aplicar la función f sobre el string leído
obtenerToken :: (String -> Token) -> AlexPosn -> String -> Token
obtenerToken f pos s = f s

-- El tipo Token:
data Token =  TkParentesisI
           |  TkParentesisD
           |  TkEntero String
           |  TkReal String
           |  TkMas
           |  TkMenos
           |  TkPor
           |  TkEntre
           |  TkElevado
           |  TkConstanteMat String
           |  TkIdentificador String
           |  TkCorcheteI
           |  TkCorcheteD    
           |  TkRango    
           |  TkIf
           |  TkAnd
           |  TkOr
           |  TkNot
           |  TkMayor
           |  TkMayorIg
           |  TkMenor
           |  TkMenorIg
           |  TkIgual
           |  TkEstilo String    
           |  TkLlaveI
           |  TkLlaveD
           |  TkWith
           |  TkPlot                          
           |  TkPushBack
           |  TkFor
           |  TkIn
           |  TkStep
           |  TkEndfor
           |  TkComilla
           |  TkComa
           |  TkPuntoYComa
           |  TkAsignacion
           |  TkFuncion String
           deriving (Eq,Show)

-- Obtiene el número de línea del AlexPosn
obtenerLinea :: (AlexPosn, Char, String) -> String
obtenerLinea (AlexPn _ x _, _, _) = show x

obtenerColumna :: (AlexPosn, Char, String) -> String
obtenerColumna (AlexPn _ _ x, _, _) = show x

obtenerError :: (AlexPosn, Char, String) -> String
obtenerError (_, _, i) = head $ lines i

-- Redefinición de alexScanTokens
--alexScanTokens :: String -> [Token]
lexer :: String -> [Token]
lexer str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                --Pendiente modificar
                AlexError e -> error $ "error lexico: " ++
                               "token inesperado '" ++ obtenerError e ++ 
                               "', linea: " ++ obtenerLinea e ++ 
                               ", columna: " ++ obtenerColumna e ++ "."
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'
}
