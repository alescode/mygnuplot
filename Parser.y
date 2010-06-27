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
    ';'             { TkPuntoYComa }
    '='             { TkAsignacion }
    "with"          { TkWith }
    "plot"          { TkPlot }
    "endfor"        { TkEndFor }
    "step"          { TkStep }
    "push_back"     { TkPushBack }
    estilo          { TkEstilo $$ }
    identificador   { TkIdentificador $$ }
    archivo         { TkArchivo $$ }

--nonassoc <?
%left "AND" "OR"
%left "NOT"
%left "=="
%nonassoc '>' '<' "<=" ">="
%left '+' '-'
%left '*' '/'
%right '^'

%%

-- Exportar "Variable"
SEC_INSTR  : INSTR ';'                                       { $1 }
           | CICLO                                           { $1 }
           | SEC_INSTR INSTR ';'                             { Secuenciacion $1 $2 }

INSTR  : identificador '(' identificador ')' '=' EM          { DefFuncion $1 $3 $6 }
       | identificador '=' EM                                { Asignacion $1 $3 }
       | "plot" ARREGLO EG "with" '[' ']'                    { GraficarVacio $2 $3 }
       | "plot" ARREGLO EG "with"
       '[' SECUENCIA_ESTILO ']'                              { GraficarArreglo $2 $3 $6 }
       | "plot" ARREGLO EG "with" estilo                     { GraficarEstilo $2 $3 (mkEstilo $5) }
       | "plot" ARREGLO EG                                   { Graficar $2 $3 }
       | "push_back" '(' identificador ',' EM ')'            { PB $3 $5 }

CICLO  : "for" identificador "in" ARREGLO
         SEC_INSTR_CICLO "endfor"                            { Ciclo $2 $4 $5 }
       | "for" identificador "in" ARREGLO 
         "step" int SEC_INSTR_CICLO "endfor"                 { CicloStep $2 $4 $6 $7 }

SEC_INSTR_CICLO : INSTR                                      { $1 }
                | CICLO                                      { $1 }
                | SEC_INSTR_CICLO ';' INSTR                  { Secuenciacion $1 $3 }

--OJO CAMBIAR UNITARIO
SECUENCIA_ESTILO  : SECUENCIA_ESTILO ',' estilo              { SecuenciaES $1 (mkEstilo $3) }
                  | estilo                                   { Unitario (mkEstilo $1) }

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
    | ARREGLO                                  { $1 }
    | "if" '(' COND ',' EM ',' EM ')'          { ExpresionCond $3 $5 $7 }

ARREGLO: '[' ']'                                       { ArregloVacio }
       | '[' SECUENCIA_EM ']'                          { ArregloEM $2 }
       | "range" '(' int ',' int ')'                   { Rango $3 $5 }
       | '[' EM "for" identificador "in" ARREGLO ']'   { ArregloComprension $2 (Variable $4) $6 }

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
      | archivo                                { Archivo $1 }

{
-- Función por Ernesto Hernández Novich
parseError :: [Token] -> a
parseError (t:ts) = error $ "Error de sintaxis en el token '" ++ show t ++
                    "', seguido de: " ++ (unlines $ map show $ take 3 ts)

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
        | Rango String String
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
                 | CicloStep String EM String Instruccion
                 | Ciclo String EM Instruccion
                 | PB String EM
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
