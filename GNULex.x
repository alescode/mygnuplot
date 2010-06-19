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
    Menos
    deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
