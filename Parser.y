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
    '+'             { TkMas }
    '-'             { TkMenos }
    '*'             { TkPor }
    '/'             { TkEntre }
    '^'             { TkElevado }
    int             { TkEntero $$ }
    real            { TkReal $$ }
    constmat        { TkConstanteMat $$ }
    funcion         { TkFuncion $$ }
    '('             { TkParentesisI }
    ')'             { TkParentesisD }
    '['             { TkCorcheteI }
    ']'             { TkCorcheteD }
    ','             { TkComa }
    "range"         { TkRango }
    "for"           { TkFor }
    "in"            { TkIn }
    "if"            { TkIf }
    "AND"           { TkAnd }
    "OR"            { TkOr }
    "NOT"           { TkNot }
    '<'             { TkMenor }
    '>'             { TkMayor }
    ">="            { TkMayorIg }
    "<="            { TkMenorIg }
    "=="            { TkIgual }
    identificador   { TkIdentificador $$ }

%left '+' '-'
%left '*' '/'
%right '^'

%%

EM  : EM '+' EM                                { Mas  $1 $3 }
    | EM '-' EM                                { Menos $1 $3 }
    | EM '*' EM                                { Por $1 $3 }
    | EM '/' EM                                { Entre $1 $3 }
    | EM '^' EM                                { Elevado $1 $3 }
    | '-' EM                                   { MenosUnario $2 }
    | '(' EM ')'                               { Expresion $2 }
    | int                                      { Entero $1 }
    | real                                     { Real $1 }
    | constmat                                 { ConstMat $1 }
    | funcion '(' EM ')'                       { Funcion $1 $3 }
    | identificador                            { Variable $1 }
    | '[' ']'                                  { ArregloVacio }
    | '[' SECUENCIA_EM ']'                     { ArregloEM $2 }
    | "range" '(' EM ',' EM ')'                { Rango $3 $5 }
    | '[' EM "for" identificador "in" EM ']'   { ArregloComprension $2 (Variable $4) $6 }
--    | "if" '(' COND ',' EM ',' EM ')'          { Condicional $3 $5 $7 }

COND  : EM                                     { ExpresionCondicion $1 }
      | COND "AND" COND                        { Conjuncion $1 $3 }
      | COND "OR" COND                         { Disyuncion $1 $3 }  
      | "NOT" COND                             { Negacion $2 } 
      | COND '>' COND                          { MayorQue $1 $3 }
      | COND '<' COND                          { MenorQue $1 $3 }
      | COND "<=" COND                         { MenorIgual $1 $3 }
      | COND ">=" COND                         { MayorIgual $1 $3 }
      | COND "==" COND                         { Igual $1 $3 }
                                                 
SECUENCIA_EM : EM                     { Unitaria $1 }
             | SECUENCIA_EM ',' EM    { Secuencia $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Variable = Var String

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
--        | Condicional COND EM EM
        deriving (Show)

-- Multiple declaration: data Condicional = Expresion EM
-- Como arreglar esto?
data Condicional = ExpresionCondicion EM
                 | Conjuncion Condicional Condicional
                 | Disyuncion Condicional Condicional
                 | Negacion Condicional
                 | MayorQue Condicional Condicional
                 | MenorQue Condicional Condicional
                 | MayorIgual Condicional Condicional
                 | MenorIgual Condicional Condicional
                 | Igual Condicional Condicional
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
