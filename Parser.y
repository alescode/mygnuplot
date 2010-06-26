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
    '\''            { TkComilla }
    ';'             { TkPuntoYComa }
    '='             { TkAsignacion }
    "with"          { TkWith }
    "plot"          { TkPlot }
    estilo          { TkEstilo $$ }
    identificador   { TkIdentificador $$ }

--%left ';'
%left "AND" "OR"
%left "NOT"
%left "=="
%left '>' '<' "<=" ">="
%left '+' '-'
%left '*' '/'
%right '^'

%%

-- Exportar "Variable"
SEC_INSTR  : INSTR                                        { $1 }
           | SEC_INSTR INSTR                              { Secuenciacion $1 $2 }

INSTR  : identificador '(' identificador ')' '=' EM ';'         { DefFuncion $1 $3 $6 }
       | identificador '=' EM ';'                               { Asignacion $1 $3 }
       | "plot" EM ',' EG "with" '[' ']' ';'                    { GraficarVacio $2 $4 }
       | "plot" EM ',' EG "with" '[' SECUENCIA_ESTILO ']' ';'   { GraficarArreglo $2 $4 $7 }
       | "plot" EM ',' EG "with" estilo ';'                     { GraficarEstilo $2 $4 (mkEstilo $6) }
       | "plot" EM ',' EG ';'                                   { Graficar $2 $4 }

--OJO CAMBIAR
SECUENCIA_ESTILO  : SECUENCIA_ESTILO ',' estilo           { SecuenciaES $1 (mkEstilo $3) }
                  | estilo                                { Unitario (mkEstilo $1) }

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
    | identificador '(' EM ')'                 { Funcion $1 $3 }
    | identificador                            { Variable $1 }
    | '[' ']'                                  { ArregloVacio }
    | '[' SECUENCIA_EM ']'                     { ArregloEM $2 }
    | "range" '(' EM ',' EM ')'                { Rango $3 $5 }
    | '[' EM "for" identificador "in" EM ']'   { ArregloComprension $2 (Variable $4) $6 }
    | "if" '(' COND ',' EM ',' EM ')'          { ExpresionCond $3 $5 $7 }

SECUENCIA_EM  : EM                     { Unitaria $1 }
              | SECUENCIA_EM ',' EM    { SecuenciaEM $1 $3 }

COND  : EM                                     { Condicion $1 }
      | COND "AND" COND                        { Conjuncion $1 $3 }
      | COND "OR" COND                         { Disyuncion $1 $3 }  
      | "NOT" COND                             { Negacion $2 } 
      | COND '>' COND                          { MayorQue $1 $3 }
      | COND '<' COND                          { MenorQue $1 $3 }
      | COND "<=" COND                         { MenorIgual $1 $3 }
      | COND ">=" COND                         { MayorIgual $1 $3 }
      | COND "==" COND                         { Igual $1 $3 }

EG    : EM                                     { Graficable $1 }
      | '\'' identificador '\''                { Archivo $2 }

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
        | ExpresionCond Condicional EM EM
        deriving (Show)

-- Multiple declaration: data Condicional = Expresion EM
-- Como arreglar esto?
data Condicional = Condicion EM
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
                     | SecuenciaEM SecuenciaExpMat EM
                     deriving (Show)

data EG = Graficable EM
        | Archivo String
        deriving (Show)

data Instruccion = DefFuncion String String EM
                 | Secuenciacion Instruccion Instruccion
                 | Asignacion String EM
                 | GraficarVacio EM EG
                 | GraficarArreglo EM EG SecuenciaEstilo 
                 | GraficarEstilo EM EG Estilo
                 | Graficar EM EG
                 deriving (Show)

data Estilo = Lineas
            | Puntos
            | LineasPunteadas 
            deriving (Show)

mkEstilo :: String -> Estilo
mkEstilo str
   | str == "lines" = Lineas
   | str == "points" = Puntos
   | str == "linespoints" = LineasPunteadas

data SecuenciaEstilo = Unitario Estilo
                     | SecuenciaES SecuenciaEstilo Estilo
                     deriving (Show)
main = do
    s <- getContents
    print $ parse $ lexer s
}   
