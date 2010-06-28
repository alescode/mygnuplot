module AS (
      module AS
) where

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
        deriving (Eq, Show)

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
                 deriving (Eq, Show)

data SecuenciaExpMat = Unitaria EM
                     | SecuenciaEM SecuenciaExpMat EM
                     deriving (Eq, Show)

data EG = Graficable EM
        | Archivo String
        deriving (Eq, Show)

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
                 deriving (Eq, Show)

data Estilo = Lineas
            | Puntos
            | LineasPunteadas 
            deriving (Eq)

data SecuenciaEstilo = Unitario Estilo
                     | SecuenciaES SecuenciaEstilo Estilo
                     deriving (Eq, Show)

mkEstilo :: String -> Estilo
mkEstilo str
   | str == "lines" = Lineas
   | str == "points" = Puntos
   | str == "linespoints" = LineasPunteadas

instance Show Estilo where
      show Lineas = "lines"
      show Puntos = "points"
      show LineasPunteadas = "linespoints"
      
{-

instance Show Prog where
      show (ExprList []) = ""
      show (ExprList (x:xs)) = show x ++ show (ExprList xs)

instance Show Expr where
      show expr = show' 0 expr
            where
                  show' n (Add exp1 exp2) = (replicate (2*n) ' ') ++ "Add\n" ++ show' (n+1) exp1 ++ show' (n+1) exp2
                  show' n (Substract exp1 exp2) = (replicate (2*n) ' ') ++ "Substract\n" ++ show' (n+1) exp1 ++ show' (n+1) exp2
                  show' n (Times exp1 exp2) = (replicate (2*n) ' ') ++ "Times\n" ++ show' (n+1) exp1 ++ show' (n+1) exp2
                  show' n (Divide exp1 exp2) = (replicate (2*n) ' ') ++ "Divide\n" ++ show' (n+1) exp1 ++ show' (n+1) exp2
                  show' n (UnaryMinus exp) = (replicate (2*n) ' ') ++ "UnaryMinus\n" ++ show' (n+1) exp
                  show' n (Num num) = (replicate (2*n) ' ') ++ "Num: " ++ show num ++ "\n"
                  show' n (Var s) = (replicate (2*n) ' ') ++ "Var: " ++ show s ++ "\n"
-}