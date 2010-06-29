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
        | Variable String
        | ArregloEM [EM]
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

data EG = Graficable EM
        | Archivo String
        deriving (Eq, Show)

data Bloque = Secuencia [Instruccion]
            deriving (Eq, Show)

data Instruccion = DefFuncion String String EM
                 | Asignacion String EM
                 | GraficarVacio EM EG
                 | Graficar EM EG [Estilo]
                 | CicloStep String EM EM Bloque
                 | Ciclo String EM Bloque
                 | PushBack String EM
                 deriving (Eq, Show)

data Estilo = Lineas
            | Puntos
            | LineasPunteadas 
            deriving (Eq)

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
showInstruccion :: Int -> Instruccion -> String
showInstruccion n (DefFuncion name var cuerpo) = (replicate (2*n) ' ')
					 							++ "Declaracion\n"
												++ (replicate (2*n + 2) ' ')
												++ name
												++ "( " ++ var ++ " )=\n"
												++ showEM (n+1) cuerpo
												++ "\n"
showInstruccion n (Secuenciacion i1 i2) = (replicate (2*n) ' ')
										  ++ showInstruccion (n+1) i1
										  ++ showInstruccion (n+1) i2
										  ++ "\n"
showInstruccion n (Asignacion v valor) = (replicate (2*n) ' ')
										++ "Asignacion " ++ v ++ "\n"
										++ showEM (n+1) valor
										++ "\n"
showInstruccion n (GraficarVacio e g) = (replicate (2*n) ' ')
										++ "Graficar\n"
										++ showEM (n+1) e
										++ showEG (n+1) g
										++ "\n"
showInstruccion n (GraficarArreglo e g estilos) = (replicate (2*n) ' ')
												 ++ "Graficar\n"
												 ++ showEM (n+1) e
												 ++ showEG (n+1) g
												 ++ showEstilos (n+1) estilos
												 ++ "\n"
showInstruccion n (GraficarEstilo e g estilo) = (replicate (2*n) ' ')
											   ++ "Graficar\n"
											   ++ showEM (n+1) e
											   ++ showEG (n+1) g
											   ++ showEstilo (n+1) estilo
											   ++ "\n"
showInstruccion n (Graficar e g) = (replicate (2*n) ' ')
								  ++ "Graficar\n"
								  ++ showEM (n+1) e
								  ++ showEG (n+1) g
								  ++ "\n"
showInstruccion n (CicloStep var exp paso instr) = (replicate (2*n) ' ')
												  ++ "Ciclo \n"
												  ++ (replicate (2*n + 2) ' ')
												  ++ var ++ " in \n"
												  ++ showEM (n+1) exp
												  ++ (replicate (2*n + 2) ' ')
												  ++ " step " ++ paso ++ "\n"
												  ++ showInstruccion (n+1) instr
												  ++ "\n"
showInstruccion n (Ciclo var exp instr) = (replicate (2*n) ' ')
										 ++ "Ciclo \n"
										 ++ (replicate (2*n + 2) ' ')					
										 ++ var ++ " in \n"
										 ++ showEM (n+1) exp
										 ++ showInstruccion (n+1) instr
										 ++ "\n"
showInstruccion n (PB var exp) = (replicate (2*n) ' ')
							   ++ "push_back " ++ var
							   ++ showEM (n+1) exp
							   ++ "\n"

showEM n _ = show ""

instance Show Instruccion where
	  show fun = showInstruccion 0 fun
-}
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
