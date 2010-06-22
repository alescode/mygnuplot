{
module Main (main) where
}

%wrapper "posn"

$digito = 0-9           -- dígitos
$alfa = [a-zA-z]        -- caracteres alfabéticos

tokens :-
    $white+                                        ;
    "#".*                                          ;
    "("                                            { par $ const ParentesisI }
    ")"                                            { par $ const ParentesisD }
    $digito+                                       { par Entero }
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

-- La función par hace que Alex devuelva tuplas con la linea
-- actual de lectura del analizador y el token leído
-- que se obtiene al aplicar la función f sobre el string leído
--par :: (String -> Token) -> AlexPosn -> String -> (Int, Token)
--par f pos str = (getPosnLine pos, f str)

-- La función par hace que Alex devuelva tuplas con el estado
-- actual de lectura del analizador (AlexPosn) y el token leído
-- que se obtiene al aplicar la función f sobre el string leído
par :: (String -> Token) -> AlexPosn -> String -> (AlexPosn, Token)
par f pos s = (pos, f s)

-- Obtiene el número de línea del AlexPosn
obtenerLinea :: AlexPosn -> Int
obtenerLinea (AlexPn _ x _) = x

obtenerColumna :: AlexPosn -> Int
obtenerColumna (AlexPn _ _ x) = x

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

-- Redefinición de alexScanTokens
--alexScanTokens :: String -> [Token]
lexer str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError e -> error $ "lexical error " ++ show e
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

main = do
  s <- getContents
  print (lexer s)
}
