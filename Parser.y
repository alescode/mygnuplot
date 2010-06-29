{
--module Parser where
module Parser (parse) where

import Lexer
import AS
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
PROGRAMA   : SEC_INSTR                                       { Secuencia $ reverse $1 }

SEC_INSTR  : INSTR ';'                                       { [$1] }
           | CICLO                                           { [$1] }
           | SEC_INSTR INSTR ';'                             { $2 : $1 }
           | SEC_INSTR CICLO                                 { $2 : $1 }

INSTR  : identificador '(' identificador ')' '=' EM          { DefFuncion $1 $3 $6 }
       | identificador '=' EM                                { Asignacion $1 $3 }
       | "plot" EM ',' EG "with" '[' ']'                     { Graficar $2 $4 []}
       | "plot" EM ',' EG "with"
       '[' SECUENCIA_ESTILO ']'                              { Graficar $2 $4 (reverse $7) }
       | "plot" EM ',' EG "with" estilo                      { Graficar $2 $4 [(mkEstilo $6)] }
       | "plot" EM ',' EG                                    { Graficar $2 $4 []}
       | "push_back" '(' identificador ',' EM ')'            { PushBack $3 $5 }

CICLO  : "for" identificador "in" EM
         SEC_INSTR_CICLO "endfor"                            { Ciclo $2 $4 (Secuencia $ reverse $5) }
       | "for" identificador "in" EM 
         "step" EM SEC_INSTR_CICLO "endfor"                 { CicloStep $2 $4 $6 (Secuencia $ reverse $7) }

SEC_INSTR_CICLO : INSTR                                      { [$1] }
                | CICLO                                      { [$1] }
                | SEC_INSTR_CICLO ';' INSTR                  { $3 : $1 }

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
    | '[' ']'                                       { (ArregloEM []) } -- Cambiar
    | '[' SECUENCIA_EM ']'                          { ArregloEM $ reverse $2 }
    | "range" '(' EM ',' EM ')'                     { Rango $3 $5 }
    | '[' EM "for" identificador "in" EM ']'        { ArregloComprension $2 (Variable $4) $6 }
    | "if" '(' COND ',' EM ',' EM ')'               { ExpresionCond $3 $5 $7 }

SECUENCIA_EM  : EM                     { [$1] }
              | SECUENCIA_EM ',' EM    { $3 : $1 }

SECUENCIA_ESTILO : estilo                                   { [mkEstilo $1] }
                 | SECUENCIA_ESTILO ',' estilo              { mkEstilo $3 : $1 }

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

parseError :: [ParserStatus] -> a
parseError (t:ts) = error $ "error sintactico en la linea: " ++ linea
                    ++ "\n\tcolumna: " ++ columna
                    ++ "\n\ten el token '" ++ show (token t) ++ "'"
                    ++ "\n\tseguido de: "
                    ++ (unwords $ map show $ take 3 ts)
                    where linea = show $ numLinea t
                          columna = show $ numCol t

-- parse :: [ParserStatus] -> Instruccion

}
