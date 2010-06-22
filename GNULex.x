{
module Main (main) where
}

%wrapper "basic"

$digito = [0-9]
$alfa = [a-zA-z]

tokens :-
    $white+                                        ;
    "#".*                                          ;
    "("                                            { \s -> ParentesisI }
    ")"                                            { \s -> ParentesisD }
    $digito+                                       { \s -> Entero s }
    $digito+ ("." $digito+)? ("e" $digito+)?       { \s -> Real s}
    [\+\-\*\/]                                     { \s -> AritmeticoBinario (head s) }
    "pi" | "e"                                     { \s -> ConstanteMat s }
    $alfa+                                         { \s -> Variable s }
    "-"                                            { \s -> Menos }
	"["											   { \s -> CorcheteI }
	"]"											   { \s -> CorcheteD }
	"range"										   { \s -> Rango }
	"if"										   { \s -> Condicional }
	"AND" | "OR" | "NOT"						   { \s -> OperadorLogico s }
	[\<\>]=? | "=="								   { \s -> OperadorRelacional s }
    "lines" | "points" | "linespoints"			   { \s -> Estilo s}
	"plot"										   { \s -> Plot }
	"{"											   { \s -> LlaveI }
	"}"											   { \s -> LlaveD }
	"with"										   { \s -> With }
	"push_back"									   { \s -> PushBack }
{
-- The token type:
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

main = do
  s <- getContents
  print (alexScanTokens s)
}
