{
module Main (main) where
}

%wrapper "basic"

$digito = [0-9]
$alfa = [a-zA-z]

tokens :-
    $white+                                        ;
    "#".*                                          ;
    "("                                            { \s -> LParen }
    ")"                                            { \s -> RParen }
    $digito+                                       { \s -> Entero (read s) }
    $digito+ ("." $digito+)? ("e" digito+)?          { \s -> Real (read s)}
    --$digito+(\.digito+|e-?digito+)                 { \s -> Real (read s) } 
    --[\+\-\*\/]                                     { \s -> Operator (head s) }

{
-- The token type:
data Token =
    LParen      |
    RParen      |
    Entero Int  |      
    Real Double |
    Mu
    deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
