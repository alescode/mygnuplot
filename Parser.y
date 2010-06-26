{
--module Parser where
module Main (main, parse, lexer) where

import Lexer
}

-- parse :: [Token] -> EM
%name parse
%tokentype { Token }
%error { parseError }

%token
    '+'          { TkMas }
    '-'          { TkMenos }
    '*'          { TkPor }
    '/'          { TkEntre }
    '^'          { TkElevado }
    int          { TkEntero $$ }
    real         { TkReal $$ }
    constmat     { TkConstanteMat $$ }
    funcion      { TkFuncion $$ }
    '('          { TkParentesisI }
    ')'          { TkParentesisD }
    '['          { TkCorcheteI }
    ']'          { TkCorcheteD }
    ','          { TkComa }
    "range"      { TkRango }
    "for"        { TkFor }
    "in"         { TkIn }
    variable     { TkIdentificador $$ }

%left '+' '-'
%left '*' '/'
%right '^'
--%right NEG

%%

EM  : EM '+' EM                              { Mas  $1 $3 }
    | EM '-' EM                              { Menos $1 $3 }
    | EM '*' EM                              { Por $1 $3 }
    | EM '/' EM                              { Entre $1 $3 }
    | EM '^' EM                              { Elevado $1 $3 }
    | '-' EM                                 { MenosUnario $2 }
    | '(' EM ')'                             { Expresion $2 }
    | int                                    { Entero $1 }
    | real                                   { Real $1 }
    | constmat                               { ConstMat $1 }
    | funcion '(' EM ')'                     { Funcion $1 $3 }
    | variable                               { Variable $1 }
    | '[' ']'                                { ArregloVacio }
    | '[' SECUENCIA_EM ']'                   { ArregloEM $2 }
    | "range" '(' EM ',' EM ')'              { Rango $3 $5 }
    | '[' EM "for" variable "in" EM ']'      { ArregloComprension $2 (Variable $4) $6 }

SECUENCIA_EM : EM                     { Unitaria $1 }
             | SECUENCIA_EM ',' EM    { Secuencia $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data EM = Expresion EM
        | Mas EM EM
        | Menos EM EM
        | MenosUnario EM
        | Por EM EM
        | Entre EM EM
        | Elevado EM EM
        | Entero String
        | Real String
        | ConstMat String
        | Funcion String EM
        | Variable String -- Como compactar?? ArregloComprension EM Variable EM
        | ArregloVacio
        | ArregloEM SecuenciaExpMat
        | Rango EM EM
        | ArregloComprension EM EM EM
        deriving (Show)

data SecuenciaExpMat = Unitaria EM
                     | Secuencia SecuenciaExpMat EM
                     deriving (Show)

--    | Minus EM EM
--    | Times EM EM
--    | Div EM EM
--    | Paren EM
--    | Negate EM
--	| Pi
--	| E
--	| Funcion EM
--    deriving (Show)

main = do
    s <- getContents
    print $ parse $ lexer s
}   
