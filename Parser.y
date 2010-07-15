{
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
--    archivo         { ParserStatus (TkArchivo $$) _ _ }

-- Precedencias
%left "OR"
%left "AND"
%right "NOT"
%left "==" '>' '<' "<=" ">="
%left '+' '-'
%left '*' '/'
%right menos_unario
%right '^'

%%

PROGRAMA      : SECUENCIA_1                            { Secuencia $
                                                             reverse $1 }

SECUENCIA_1   : INSTR                                  { [$1] }
              | CICLO                                  { [$1] }
              | SECUENCIA_2 INSTR                      { $2 : $1 }   
              | SECUENCIA_2 CICLO                      { $2 : $1 }   
                                                          
SECUENCIA_2   : INSTR ';'                              { [$1] }   
              | CICLO                                  { [$1] }   
              | SECUENCIA_2 INSTR ';'                  { $2 : $1 }
              | SECUENCIA_2 CICLO                      { $2 : $1 }
                                                               
INSTR  : identificador '(' identificador ')' '=' EM    { DefFuncion $1
                                                             (Variable $3) $6 }
       | identificador '=' EM                          { Asignacion
                                                             (Variable $1) $3 }
       | "plot" EM ',' EM "with" '[' ']'               { GraficarEstilo 
                                                             $2 $4 []}
       | "plot" EM ',' EM "with"
       '[' SECUENCIA_ESTILO ']'                        { GraficarEstilo $2 $4
                                                         (reverse $7) }
       | "plot" EM ',' EM "with" estilo                { GraficarEstilo $2 
                                                         $4 [(readEstilo $6)] }
       | "plot" EM ',' EM                              { Graficar $2 $4 }
       | "push_back" '(' identificador ',' EM ')'      { PushBack 
                                                             (Variable $3) $5 }

CICLO  : "for" identificador "in" EM
         SECUENCIA_1 "endfor"                          { Ciclo (Variable $2) 
                                                             $4 (Secuencia $
                                                             reverse $5) }
       | "for" identificador "in" EM 
         "step" EM SECUENCIA_1 "endfor"                { CicloStep (Variable $2)
                                                          $4 $6 (Secuencia $
                                                          reverse $7) }

EM  : EM '+' EM                                     { Suma  $1 $3 }
    | EM '-' EM                                     { Resta $1 $3 }
    | EM '*' EM                                     { Multiplicacion $1 $3 }
    | EM '/' EM                                     { Division $1 $3 }
    | EM '^' EM                                     { Potencia $1 $3 }
    | '-' EM  %prec menos_unario                    { Menos $2 }
    | '(' EM ')'                                    { $2 }
    | int                                           { Entero $1 }
    | real                                          { Real $1 }
    | funcion '(' EM ')'                            { EMLlamada 
                                                        (LlamadaFuncion $1 $3) }
    | identificador '(' EM ')'                      { EMLlamada 
                                                        (LlamadaFuncion $1 $3) }
    | identificador                                 { EMVariable (Variable $1) }
    | '[' ']'                                       { ArregloEM [] }
    | '[' SECUENCIA_EM ']'                          { ArregloEM $ reverse $2 }
    | "range" '(' EM ',' EM ')'                     { Rango $3 $5 }
    | '[' EM "for" identificador "in" EM ']'        { ArregloComprension $2
                                                         (Variable $4) $6 }
    | "if" '(' COND ',' EM ',' EM ')'               { ExpresionCond $3 $5 $7 }

SECUENCIA_EM  : EM                     { [$1] }
              | SECUENCIA_EM ',' EM    { $3 : $1 }

SECUENCIA_COND  : COND                       { [$1] }
                | SECUENCIA_COND ',' COND    { $3 : $1 }

SECUENCIA_ESTILO : estilo                           { [readEstilo $1] }
                 | SECUENCIA_ESTILO ',' estilo      { readEstilo $3 : $1 }

COND  : COND '+' COND                               { CSuma $1 $3 }
      | COND '-' COND                               { CResta $1 $3 }
      | COND '*' COND                               { CMultiplicacion $1 $3 }
      | COND '/' COND                               { CDivision $1 $3 }
      | COND '^' COND                               { CPotencia $1 $3 }
      | '-' COND  %prec menos_unario                { CMenos $2 }
      | '(' COND ')'                                { $2 }
      | int                                         { CEntero $1 }
      | real                                        { CReal $1 }
      | funcion '(' COND ')'                        { CondicionalLlamada
                                                     (CLlamadaFuncion $1 $3) }
      | identificador '(' COND ')'                  { CondicionalLlamada 
                                                     (CLlamadaFuncion $1 $3) }
      | identificador                               { CondicionalVariable 
                                                          (Variable $1) }
      | '[' ']'                                     { ArregloCondicional [] }
      | '[' SECUENCIA_COND ']'                      { ArregloCondicional $ 
                                                          reverse $2 }
      | "range" '(' COND ',' COND ')'               { CRango $3 $5 }
      | '[' COND "for" identificador "in" COND ']'  { CArregloComprension $2
                                                          (Variable $4) $6 }
      | "if" '(' COND ',' COND ',' COND ')'         { CExpresionCond $3 $5 $7 } 
      | COND "AND" COND                           { Conjuncion $1 $3 }
      | COND "OR" COND                            { Disyuncion $1 $3 }  
      | "NOT" COND                                { Negacion $2 } 
      | COND '>' COND                             { MayorQue $1 $3 }
      | COND '<' COND                             { MenorQue $1 $3 }
      | COND "<=" COND                            { MenorIgual $1 $3 }
      | COND ">=" COND                            { MayorIgual $1 $3 }
      | COND "==" COND                            { Igual $1 $3 }

{

-- Funcion de error estandar
parseError :: [ParserStatus] -> a
parseError [_] = error $ "error sintactico en el ultimo token, antes del fin de archivo"
parseError (t:ts) = error $ "error sintactico en la linea: " ++ linea
                    ++ "\n\tcolumna: " ++ columna
                    ++ "\n\ten el token '" ++ show (token t) ++ "'"
                    ++ "\n\tseguido de: "
                    ++ (unwords $ map show $ take 3 ts)
                    where linea = show $ numLinea t
                          columna = show $ numCol t

-- Funcion del analizador (definida por Happy)
parse :: [ParserStatus] -> Bloque

}
