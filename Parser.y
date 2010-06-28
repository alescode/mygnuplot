{
--module Parser where
module Main (parse) where

import Lexer
}

%name parse
%tokentype { ParserStatus }
%error { parseError }

%token
    '+'             { ParserStatus TkMas _ _ }
    '-'             { ParserStatus TkMenos _ _ }
    '*'             { ParserStatus TkPor _ _ }
    '/'             { ParserStatus TkEntre  _ _ }
    '^'             { ParserStatus TkElevado _ _ }
    int             { ParserStatus (TkEntero $$) _ _ }
    real            { ParserStatus (TkReal $$) _ _ }
    constmat        { ParserStatus (TkConstanteMat $$) _ _ }
    funcion         { ParserStatus (TkFuncion $$) _ _ }
    '('             { ParserStatus TkParentesisI _ _ }
    ')'             { ParserStatus TkParentesisD _ _ }
    '['             { ParserStatus TkCorcheteI _ _ }
    ']'             { ParserStatus TkCorcheteD _ _ }
    ','             { ParserStatus TkComa _ _ }
    "range"         { ParserStatus TkRango _ _ }
    "for"           { ParserStatus TkFor _ _ }
    "in"            { ParserStatus TkIn _ _ }
    "if"            { ParserStatus TkIf _ _ }
    "AND"           { ParserStatus TkAnd _ _ }
    "OR"            { ParserStatus TkOr _ _ }
    "NOT"           { ParserStatus TkNot _ _ }
    '<'             { ParserStatus TkMenor _ _ }
    '>'             { ParserStatus TkMayor _ _ }
    ">="            { ParserStatus TkMayorIg _ _ }
    "<="            { ParserStatus TkMenorIg _ _ }
    "=="            { ParserStatus TkIgual _ _ }
    ';'             { ParserStatus TkPuntoYComa _ _ }
    '='             { ParserStatus TkAsignacion _ _ }
    "with"          { ParserStatus TkWith _ _ }
    "plot"          { ParserStatus TkPlot _ _ }
    "endfor"        { ParserStatus TkEndFor _ _ }
    "step"          { ParserStatus TkStep _ _ }
    "push_back"     { ParserStatus TkPushBack _ _ }
    estilo          { ParserStatus (TkEstilo $$) _ _ }
    identificador   { ParserStatus (TkIdentificador $$) _ _ }
    archivo         { ParserStatus (TkArchivo $$) _ _ }

--nonassoc <?
%left "OR"
%left "AND"
%right "NOT"
%nonassoc "==" '>' '<' "<=" ">="
%left '+' '-'
%left '*' '/'
%right menos_unario
%right '^'

%%

-- Exportar "Variable"
SEC_INSTR  : INSTR ';'                                       { $1 }
           | CICLO                                           { $1 }
           | SEC_INSTR INSTR ';'                             { Secuenciacion $1 $2 }
           | SEC_INSTR CICLO                                 { Secuenciacion $1 $2 }

INSTR  : identificador '(' identificador ')' '=' EM          { DefFuncion $1 $3 $6 }
       | identificador '=' EM                                { Asignacion $1 $3 }
       | "plot" EM ',' EG "with" '[' ']'                     { GraficarVacio $2 $4 }
       | "plot" EM ',' EG "with"
       '[' SECUENCIA_ESTILO ']'                              { GraficarArreglo $2 $4 $7 }
       | "plot" EM ',' EG "with" estilo                      { GraficarEstilo $2 $4 (mkEstilo $6) }
       | "plot" EM ',' EG                                    { Graficar $2 $4 }
       | "push_back" '(' identificador ',' EM ')'            { PB $3 $5 }

CICLO  : "for" identificador "in" EM
         SEC_INSTR_CICLO "endfor"                            { Ciclo $2 $4 $5 }
       | "for" identificador "in" EM 
         "step" int SEC_INSTR_CICLO "endfor"                 { CicloStep $2 $4 $6 $7 }

SEC_INSTR_CICLO : INSTR                                      { $1 }
                | CICLO                                      { $1 }
                | SEC_INSTR_CICLO ';' INSTR                  { Secuenciacion $1 $3 }

--OJO CAMBIAR UNITARIO
SECUENCIA_ESTILO  : SECUENCIA_ESTILO ',' estilo              { SecuenciaES $1 (mkEstilo $3) }
                  | estilo                                   { Unitario (mkEstilo $1) }

EM  : EM '+' EM                                     { Suma  $1 $3 }
    | EM '-' EM                                     { Resta $1 $3 }
    | EM '*' EM                                     { Multiplicacion $1 $3 }
    | EM '/' EM                                     { Division $1 $3 }
    | EM '^' EM                                     { Potencia $1 $3 }
    | '-' EM  %prec menos_unario                    { Menos $2 }
    | '(' EM ')'                                    { $2 }
    | int                                           { Entero $1 }
    | real                                          { Real $1 }
    | constmat                                      { ConstMat $1 }
    | funcion '(' EM ')'                            { Funcion $1 $3 }
    | identificador '(' EM ')'                      { Funcion $1 $3 }
    | identificador                                 { Variable $1 }
    | '[' ']'                                       { ArregloVacio }
    | '[' SECUENCIA_EM ']'                          { ArregloEM $2 }
    | "range" '(' EM ',' EM ')'                     { Rango $3 $5 }
    | '[' EM "for" identificador "in" EM ']'        { ArregloComprension $2 (Variable $4) $6 }
    | "if" '(' COND ',' EM ',' EM ')'               { ExpresionCond $3 $5 $7 }

SECUENCIA_EM  : EM                     { Unitaria $1 }
              | SECUENCIA_EM ',' EM    { SecuenciaEM $1 $3 }

COND  : EM                                     { Condicion $1 }
      | COND "AND" COND                        { Conjuncion $1 $3 }
      | COND "OR" COND                         { Disyuncion $1 $3 }  
      | "NOT" COND                             { Negacion $2 } 
      | EM '>' EM                              { MayorQue $1 $3 }
      | EM '<' EM                              { MenorQue $1 $3 }
      | EM "<=" EM                             { MenorIgual $1 $3 }
      | EM ">=" EM                             { MayorIgual $1 $3 }
      | EM "==" EM                             { Igual $1 $3 }

EG    : EM                                     { Graficable $1 }
      | archivo                                { Archivo $1 }

{

-- Función por Ernesto Hernández Novich
parseError :: [ParserStatus] -> a
parseError (t:ts) = error $ "error sintactico en la linea: " ++ linea
                    ++ "\n\tcolumna: " ++ columna
                    ++ "\n\ten el token '" ++ show (token t) ++ "'"
                    ++ "\n\tseguido de: "
                    ++ (unwords $ map show $ take 3 ts)
                    where linea = show $ numLinea t
                          columna = show $ numCol t

data Variable = Var String

data EM = Suma EM EM
        | Resta EM EM
        | Menos EM
        | Multiplicacion EM EM
        | Division EM EM
        | Potencia EM EM
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
                 | MayorQue EM EM
                 | MenorQue EM EM
                 | MayorIgual EM EM
                 | MenorIgual EM EM
                 | Igual EM EM
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

-- parse :: [Token] -> Instruccion

main = do
    s <- getContents
    return ()
    print $ parse $ lexer s 
}   
