{
module Main (main) where
}

%wrapper "posn"

$digito = 0-9           -- dígitos
$alfa = [a-zA-Z]        -- caracteres alfabéticos

tokens :-
    $white+                                        ;
    "#".*                                          ;
    "("                                            { obtenerToken $ const ParentesisI }
    ")"                                            { obtenerToken $ const ParentesisD }
    $digito+                                       { obtenerToken Entero }
    $digito+ ("." $digito+)? ("e" $digito+)?       { obtenerToken Real }
    ("." $digito+) ("e" $digito+)?				   { obtenerToken Real }
    [\+\-\*\/\^]                                   { obtenerToken OperadorAritmetico } 
    "pi" | "e"                                     { obtenerToken ConstanteMat }
    "-"                                            { obtenerToken $ const Menos }
	"["											   { obtenerToken $ const CorcheteI }
	"]"											   { obtenerToken $ const CorcheteD }
	"range"										   { obtenerToken $ const Rango }
	"if"										   { obtenerToken $ const If }
	"AND" | "OR" | "NOT"						   { obtenerToken OperadorLogico }
	[\<\>]=? | "=="								   { obtenerToken OperadorRelacional }
    "lines" | "points" | "linespoints"			   { obtenerToken Estilo }
	"plot"										   { obtenerToken $ const Plot }
	\{											   { obtenerToken $ const LlaveI }
	\}											   { obtenerToken $ const LlaveD }
	"with"										   { obtenerToken $ const With }
	"push_back"									   { obtenerToken $ const PushBack }
	"for"										   { obtenerToken $ const For }
	"in"										   { obtenerToken $ const In }
	"step"										   { obtenerToken $ const Step }
	"endfor"									   { obtenerToken $ const Endfor }
	\'											   { obtenerToken $ const Comilla }
	\;											   { obtenerToken $ const PuntoYComa }
	\,											   { obtenerToken $ const Coma }
	\=											   { obtenerToken $ const Asignacion }
	"sin" | "cos" | "tan" | "exp" | "log" |
    "ceil" | "floor"                               { obtenerToken Funcion }
	$alfa+                                         { obtenerToken Identificador }

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
data Token =  ParentesisI
		   |  ParentesisD
		   |  Entero String
		   |  Real String
		   |  OperadorAritmetico String
		   |  ConstanteMat String
		   |  Identificador String
		   |  Menos	
		   |  CorcheteI
		   |  CorcheteD	
		   |  Rango	
		   |  If
		   |  OperadorLogico String
		   |  OperadorRelacional String	
		   |  Estilo String	
		   |  LlaveI
		   |  LlaveD
		   |  With
		   |  Plot						  
		   |  PushBack
		   |  For
		   |  In
		   |  Step
		   |  Endfor
		   |  Comilla
		   |  Coma
		   |  PuntoYComa
		   |  Asignacion
		   |  Funcion String
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
lexer str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError e -> error $ "error lexico: " ++
                               "token inesperado '" ++ obtenerError e ++ 
                               "', linea: " ++ obtenerLinea e ++ 
                               ", columna: " ++ obtenerColumna e ++ "."
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

main = do
  s <- getContents
  print (lexer s)
}
