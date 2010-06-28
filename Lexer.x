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

data ParserStatus = ParserStatus { token :: Token
                                 , numLinea :: Int
                                 , numCol :: Int
                                 }

instance Show ParserStatus where
    show (ParserStatus x _ _) = show x

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
