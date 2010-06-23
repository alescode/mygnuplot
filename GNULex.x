{
module Main (main) where
}

%wrapper "posn"

$digito = 0-9           -- dígitos
$alfa = [a-zA-z]        -- caracteres alfabéticos

tokens :-
    $white+                                        ;
    "#".*                                          ;
    "("                                            { obtenerToken $ const ParentesisI }
    ")"                                            { obtenerToken $ const ParentesisD }
    $digito+                                       { obtenerToken Entero }
--    $digito+ ("." $digito+)? ("e" $digito+)?       { tok (\p s -> Real s) }
--    [\+\-\*\/]                                     { tok (\p s -> AritmeticoBinario (head s)) }
--    "pi" | "e"                                     { tok (\p s -> ConstanteMat s) }
--    $alfa+                                         { tok (\p s -> Variable s) }
--    "-"                                            { tok (\p s -> Menos) }
--	"["											   { tok (\p s -> CorcheteI) }
--	"]"											   { tok (\p s -> CorcheteD) }
--	"range"										   { tok (\p s -> Rango) }
--	"if"										   { tok (\p s -> Condicional) }
--	"AND" | "OR" | "NOT"						   { tok (\p s -> OperadorLogico s) }
--	[\<\>]=? | "=="								   { tok (\p s -> OperadorRelacional s) }
--    "lines" | "points" | "linespoints"			   { tok (\p s -> Estilo s) }
--	"plot"										   { tok (\p s -> Plot) }
--	"{"											   { tok (\p s -> LlaveI) }
--	"}"											   { tok (\p s -> LlaveD) }
--	"with"										   { tok (\p s -> With) }
--	"push_back"									   { tok (\p s -> PushBack) }

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
data Token =
    ParentesisI                   |
    ParentesisD                   |
    Entero String                 |      
    Real String                   |    
    AritmeticoBinario Char        |
    ConstanteMat String           | 
    Variable String               |
    Menos						  |
	CorcheteI					  |
	CorcheteD					  |
	Rango						  |
	Condicional					  |
	OperadorLogico String		  |
	OperadorRelacional String	  |
	Estilo String				  |
	LlaveI						  |
	LlaveD						  |
	With						  |
	Plot						  |
	PushBack
    deriving (Eq,Show)

-- Obtiene el número de línea del AlexPosn
obtenerLinea :: (AlexPosn, Char, String) -> String
obtenerLinea (AlexPn _ x _, _, _) = show x

obtenerColumna :: (AlexPosn, Char, String) -> String
obtenerColumna (AlexPn _ _ x, _, _) = show x

-- Redefinición de alexScanTokens
--alexScanTokens :: String -> [Token]
lexer str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError e -> error $ "error lexico: linea: " ++ obtenerLinea e ++ 
                               ", columna: " ++ obtenerColumna e ++ "."
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

main = do
  s <- getContents
  print (lexer s)
}
