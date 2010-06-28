{
module Main (
    module Main
) where
}

%wrapper "posn"

$digito = 0-9           -- dígitos
$alfa = [a-zA-Z]        -- caracteres alfabéticos
$mm = [\+\-]            -- un símbolo más (+) o menos (-)
$carch = [a-zA-Z\/\.\ ]           -- archivo de UNIX

tokens :-
    $white+                                          ;
    "#".*                                            ;
    \(                                               { obtenerEstado $ const TkParentesisI }
    \)                                               { obtenerEstado $ const TkParentesisD }
    $digito+                                         { obtenerEstado TkEntero }
    $digito+ ("." $digito+)? ("e" $mm? $digito+)?    { obtenerEstado TkReal }
    ("." $digito+) ("e" $mm? $digito+)?              { obtenerEstado TkReal }
    \+                                               { obtenerEstado $ const TkMas }
    \-                                               { obtenerEstado $ const TkMenos } 
    \*                                               { obtenerEstado $ const TkPor } 
    \/                                               { obtenerEstado $ const TkEntre } 
    \^                                               { obtenerEstado $ const TkElevado } 
    "pi" | "e"                                       { obtenerEstado TkConstanteMat }
    \[                                               { obtenerEstado $ const TkCorcheteI }
    \]                                               { obtenerEstado $ const TkCorcheteD }
    "range"                                          { obtenerEstado $ const TkRango }
    "if"                                             { obtenerEstado $ const TkIf }
    "AND"                                            { obtenerEstado $ const TkAnd }
    "OR"                                             { obtenerEstado $ const TkOr }
    "NOT"                                            { obtenerEstado $ const TkNot }
    "<"                                              { obtenerEstado $ const TkMenor }
    "<="                                             { obtenerEstado $ const TkMenorIg }
    ">"                                              { obtenerEstado $ const TkMayor }
    ">="                                             { obtenerEstado $ const TkMayorIg }
    "=="                                             { obtenerEstado $ const TkIgual }
    "lines" "points"? | "points"                     { obtenerEstado TkEstilo }
    "plot"                                           { obtenerEstado $ const TkPlot }
    "with"                                           { obtenerEstado $ const TkWith }
    "push_back"                                      { obtenerEstado $ const TkPushBack }
    "for"                                            { obtenerEstado $ const TkFor }
    "in"                                             { obtenerEstado $ const TkIn }
    "step"                                           { obtenerEstado $ const TkStep }
    "endfor"                                         { obtenerEstado $ const TkEndFor }
    \;                                               { obtenerEstado $ const TkPuntoYComa }
    \,                                               { obtenerEstado $ const TkComa }
    \=                                               { obtenerEstado $ const TkAsignacion }
    "sin" | "cos" | "tan" | "exp" | "log" |
    "ceil" | "floor"                                 { obtenerEstado TkFuncion }
    $alfa+                                           { obtenerEstado TkIdentificador }
    \'$carch*\'                                      { obtenerEstado TkArchivo }
    --obtener solo lo que esta entre comillas
{
-- Todas las partes derechas tienen tipo (String -> Token),
-- especifica cuál es la función para convertir una cadena de
-- caracteres en un Token

-- La función obtenerEstado hace que Alex devuelva tuplas con la linea
-- actual de lectura del analizador y el token leído
-- que se obtiene al aplicar la función f sobre el string leído
--obtenerEstado :: (String -> Token) -> AlexPosn -> String -> (Int, Token)
--obtenerEstado f pos str = (getPosnLine pos, f str)

-- La función obtenerEstado hace que Alex devuelva tuplas con el estado
-- actual de lectura del analizador (AlexPosn) y el token leído
-- que se obtiene al aplicar la función f sobre el string leído

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
           |  TkWith
           |  TkPlot                          
           |  TkPushBack
           |  TkFor
           |  TkIn
           |  TkStep
           |  TkEndFor
           |  TkComa
           |  TkPuntoYComa
           |  TkAsignacion
           |  TkFuncion String
           |  TkArchivo String
           deriving (Eq, Show)

data ParserStatus = ParserStatus { token :: Token
                                 , numLinea :: Int
                                 , numCol :: Int
                                 }

instance Show ParserStatus where
    show (ParserStatus x _ _) = show x

-- Funciones sobre AlexPosn
obtenerLinea :: AlexPosn -> Int
obtenerLinea (AlexPn _ x _) = x

obtenerColumna :: AlexPosn -> Int
obtenerColumna (AlexPn _ _ x) = x

-- Funciones para manejo de errores en Alex
-- AlexError = (AlexPosn, Char, String)
obtTokenError :: (AlexPosn, Char, String) -> String
obtTokenError (_, _, i) = head $ lines i

obtLineaError :: (AlexPosn, Char, String) -> String
obtLineaError (x, _, _) = show $ obtenerLinea x

obtColError :: (AlexPosn, Char, String) -> String
obtColError (x, _, _) = show $ obtenerColumna x

-- Funcion para devolver los tokens junto
-- con el estado del analizador
obtenerEstado :: (String -> Token) -> AlexPosn -> String -> ParserStatus
obtenerEstado f pos s = ParserStatus (f s) (obtenerLinea pos) (obtenerColumna pos)

-- Redefinición de alexScanTokens
--alexScanTokens :: String -> [ParserStatus]
lexer :: String -> [ParserStatus]
lexer str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                --Pendiente modificar
                AlexError e -> error $ "error lexico: " ++
                               "token inesperado '" ++ obtTokenError e ++ 
                               "', linea: " ++ obtLineaError e ++ 
                               ", columna: " ++ obtColError e ++ "."
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

main = do
     s <- getContents
     print $ lexer s

}
