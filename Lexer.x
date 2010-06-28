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
--    \)                                               { obtenerToken $ const TkParentesisD }
--    $digito+                                         { obtenerToken TkEntero }
--    $digito+ ("." $digito+)? ("e" $mm? $digito+)?    { obtenerToken TkReal }
--    ("." $digito+) ("e" $mm? $digito+)?              { obtenerToken TkReal }
--    \+                                               { obtenerToken $ const TkMas }
--    \-                                               { obtenerToken $ const TkMenos } 
--    \*                                               { obtenerToken $ const TkPor } 
--    \/                                               { obtenerToken $ const TkEntre } 
--    \^                                               { obtenerToken $ const TkElevado } 
--    "pi" | "e"                                       { obtenerToken TkConstanteMat }
--    \[                                               { obtenerToken $ const TkCorcheteI }
--    \]                                               { obtenerToken $ const TkCorcheteD }
--    "range"                                          { obtenerToken $ const TkRango }
--    "if"                                             { obtenerToken $ const TkIf }
--    "AND"                                            { obtenerToken $ const TkAnd }
--    "OR"                                             { obtenerToken $ const TkOr }
--    "NOT"                                            { obtenerToken $ const TkNot }
--    "<"                                              { obtenerToken $ const TkMenor }
--    "<="                                             { obtenerToken $ const TkMenorIg }
--    ">"                                              { obtenerToken $ const TkMayor }
--    ">="                                             { obtenerToken $ const TkMayorIg }
--    "=="                                             { obtenerToken $ const TkIgual }
--    "lines" "points"? | "points"                     { obtenerToken TkEstilo }
--    "plot"                                           { obtenerToken $ const TkPlot }
--    "with"                                           { obtenerToken $ const TkWith }
--    "push_back"                                      { obtenerToken $ const TkPushBack }
--    "for"                                            { obtenerToken $ const TkFor }
--    "in"                                             { obtenerToken $ const TkIn }
--    "step"                                           { obtenerToken $ const TkStep }
--    "endfor"                                         { obtenerToken $ const TkEndFor }
--    \;                                               { obtenerToken $ const TkPuntoYComa }
--    \,                                               { obtenerToken $ const TkComa }
--    \=                                               { obtenerToken $ const TkAsignacion }
--    "sin" | "cos" | "tan" | "exp" | "log" |
--    "ceil" | "floor"                                 { obtenerToken TkFuncion }
--    $alfa+                                           { obtenerToken TkIdentificador }
--    \'$carch*\'                                      { obtenerToken TkArchivo }
    --obtener solo lo que esta entre comillas
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

data ParserStatus = ParserStatus { token :: Token
                                 , numLinea :: Int
                                 , numCol :: Int
                                 }
                                 deriving (Show)

obtenerToken :: (String -> Token) -> AlexPosn -> String -> Token
obtenerToken f pos s = f s

par :: (String -> Token) -> AlexPosn -> String -> (AlexPosn, Token)
par f pos s = (pos, f s)

obtenerLinea :: AlexPosn -> Int
obtenerLinea (AlexPn _ x _) = x

obtenerColumna :: AlexPosn -> Int
obtenerColumna (AlexPn _ _ x) = x

obtenerEstado :: (String -> Token) -> AlexPosn -> String -> ParserStatus
obtenerEstado f pos s = ParserStatus (f s) (obtenerLinea pos) (obtenerColumna pos)

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

-- Happy documentation
data E a = Ok a | Failed String
         deriving(Show)

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> Ok a
      Failed e -> k e

--type LineNumber = Int
--type P a = String -> LineNumber -> E a
--
--getLineNo :: P LineNumber
--getLineNo = \s l -> Ok l

-- Obtiene el número de línea del AlexPosn

obtenerError :: (AlexPosn, Char, String) -> String
obtenerError (_, _, i) = head $ lines i

-- Redefinición de alexScanTokens
--alexScanTokens :: String -> [Token]
lexer :: String -> [ParserStatus]
lexer str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                --Pendiente modificar
                AlexError _ -> error "Hola"
                             --   $ "error lexico: " ++
                             --  "token inesperado '" ++ obtenerError e ++ 
                             --  "', linea: " ++ obtenerLinea e ++ 
                             --  ", columna: " ++ obtenerColumna e ++ "."
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

main = do
     s <- getContents
     print $ lexer s

}
