{
module Lexer (
    module Lexer
) where
}

%wrapper "posn"

$digito = 0-9                     -- dígitos
$alfa = [a-zA-Z]                  -- caracteres alfabéticos
$mm = [\+\-]                      -- un símbolo más (+) o menos (-)
$carch = [a-zA-Z\/\.\ ]           -- archivo de UNIX

tokens :-
    $white+                                          ;
    "#".*                                            ;
    \(                                               { obtenerEstado $ 
                                                           const TkParentesisI }
    \)                                               { obtenerEstado $
                                                           const TkParentesisD }
    $digito+                                         { obtenerEstado TkEntero }
    $digito+ ("." $digito+)? ("e" $mm? $digito+)?    { obtenerEstado TkReal }
    ("." $digito+) ("e" $mm? $digito+)?              { obtenerEstado TkReal }
    \+                                               { obtenerEstado $
                                                           const TkMas }
    \-                                               { obtenerEstado $
                                                           const TkMenos } 
    \*                                               { obtenerEstado $
                                                           const TkPor } 
    \/                                               { obtenerEstado $ 
                                                           const TkEntre } 
    \^                                               { obtenerEstado $
                                                           const TkElevado } 
    "pi" | "e"                                       { obtenerEstado 
                                                           TkConstanteMat }
    \[                                               { obtenerEstado $
                                                           const TkCorcheteI }
    \]                                               { obtenerEstado $
                                                           const TkCorcheteD }
    "range"                                          { obtenerEstado $
                                                           const TkRango }
    "if"                                             { obtenerEstado $
                                                           const TkIf }
    "AND"                                            { obtenerEstado $
                                                           const TkAnd }
    "OR"                                             { obtenerEstado $
                                                           const TkOr }
    "NOT"                                            { obtenerEstado $
                                                           const TkNot }
    "<"                                              { obtenerEstado $
                                                           const TkMenor }
    "<="                                             { obtenerEstado $
                                                           const TkMenorIg }
    ">"                                              { obtenerEstado $
                                                           const TkMayor }
    ">="                                             { obtenerEstado $
                                                           const TkMayorIg }
    "=="                                             { obtenerEstado $
                                                           const TkIgual }
    "lines" "points"? | "points"                     { obtenerEstado TkEstilo }
    "plot"                                           { obtenerEstado $
                                                           const TkPlot }
    "with"                                           { obtenerEstado $
                                                           const TkWith }
    "push_back"                                      { obtenerEstado $
                                                           const TkPushBack }
    "for"                                            { obtenerEstado $
                                                           const TkFor }
    "in"                                             { obtenerEstado $
                                                           const TkIn }
    "step"                                           { obtenerEstado $
                                                           const TkStep }
    "endfor"                                         { obtenerEstado $
                                                           const TkEndFor }
    \;                                               { obtenerEstado $
                                                           const TkPuntoYComa }
    \,                                               { obtenerEstado $
                                                           const TkComa }
    \=                                               { obtenerEstado $
                                                           const TkAsignacion }
    "sin" | "cos" | "tan" | "exp" | "log" |
    "ceil" | "floor"                                 { obtenerEstado TkFuncion }
    $alfa+                                           { obtenerEstado
                                                           TkIdentificador }
    \'[^']*\'                                  { obtenerEstado
                                                     (TkArchivo . init . tail) }
    .                                                { errorLexico }
    --obtener solo lo que esta entre comillas
{

{- Todas las partes derechas tienen tipo (String -> Token)
-- Funcion para devolver los tokens junto
-- con el estado del analizador -}

{- La función obtenerEstado hace que Alex devuelva tuplas con la linea
-- actual de lectura del analizador y el token leído
-- que se obtiene al aplicar la función f sobre el string leído -}
obtenerEstado :: (String -> Token) -> AlexPosn -> String -> ParserStatus
obtenerEstado f pos s = ParserStatus (f s) 
                                     (obtenerLinea pos) 
                                     (obtenerColumna pos)

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
           deriving (Eq)

instance Show Token where
    show TkParentesisI = "("
    show TkParentesisD = ")"
    show TkMas = "+"
    show TkMenos = "-"
    show TkPor = "*"
    show TkEntre = "/"
    show TkElevado = "^"
    show TkCorcheteI = "["
    show TkCorcheteD = "]"
    show TkRango = "range"
    show TkIf = "if"
    show TkAnd = "AND"
    show TkOr = "OR"
    show TkNot = "NOT"
    show TkMayor = ">"
    show TkMayorIg = ">="
    show TkMenor = "<"
    show TkMenorIg = "<="
    show TkIgual = "=="
    show TkWith = "with"
    show TkPlot = "plot"
    show TkPushBack = "push_back"
    show TkFor = "for"
    show TkIn = "in"
    show TkStep = "step"
    show TkEndFor = "endfor"
    show TkComa = ","
    show TkPuntoYComa = ";"
    show TkAsignacion = "="
    show (TkFuncion s) = s
    show (TkArchivo s) = s
    show (TkEstilo s) = s
    show (TkIdentificador s) = s
    show (TkConstanteMat s) = s
    show (TkReal s) = s
    show (TkEntero s) = s

-- El estado del parser contiene el token actual,
-- el numero de linea y de columna que estan siendo analizados
data ParserStatus = ParserStatus { token :: Token
                                 , numLinea :: Int
                                 , numCol :: Int
                                 }
                  deriving (Eq)

instance Show ParserStatus where
    show (ParserStatus x _ _) = show x

-- Funciones sobre AlexPosn
obtenerLinea :: AlexPosn -> Int
obtenerLinea (AlexPn _ x _) = x

obtenerColumna :: AlexPosn -> Int
obtenerColumna (AlexPn _ _ x) = x

errorLexico :: AlexPosn -> String -> a
errorLexico pos s = error $ "error lexico en la linea: " ++ linea
                    ++ "\n\tcolumna: " ++ columna
                    ++ "\n\ttoken inesperado '" ++ s ++ "'"
                    where linea = show $ obtenerLinea pos
                          columna = show $ obtenerColumna pos

-- Redefinición de alexScanTokens
--alexScanTokens :: String -> [ParserStatus]
lexer = alexScanTokens
}
