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
        deriving (Eq)

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
                 deriving (Eq)

data EG = Graficable EM
        | Archivo String
        deriving (Eq)

data Bloque = Secuencia [Instruccion]
            deriving (Eq)

data Instruccion = DefFuncion String String EM
                 | Asignacion String EM
				 | Graficar EM EG
                 | GraficarEstilo EM EG [Estilo]
                 | CicloStep String EM EM Bloque
                 | Ciclo String EM Bloque
                 | PushBack String EM
                 deriving (Eq)

data Estilo = Lineas
            | Puntos
            | LineasPunteadas 
            deriving (Eq)

mkEstilo :: String -> Estilo
mkEstilo str
   | str == "lines" = Lineas
   | str == "points" = Puntos
   | str == "linespoints" = LineasPunteadas

instance Show EM where
	show em = showEM 0 em

instance Show EG where
	show e = showEG 0 e

instance Show Estilo where
      show Lineas = "lines"
      show Puntos = "points"
      show LineasPunteadas = "linespoints"

instance Show Bloque where
	  show bloque = showBloque 0 bloque

instance Show Condicional where
	show cond = showCond 0 cond

showEM :: Int -> EM -> String
showEM n (Suma i d) = (replicate (2*n) ' ') ++ "Suma:\n"
					++ showEM (n+1) i
					++ showEM (n+1) d

showEM n (Resta i d) = (replicate (2*n) ' ') ++ "Resta:\n"
					  ++ showEM (n+1) i
					  ++ showEM (n+1) d

showEM n (Menos e) = (replicate (2*n) ' ') ++ "Menos:\n"
					++ showEM (n+1) e

showEM n (Multiplicacion i d) = (replicate (2*n) ' ') ++ "Multiplicacion:\n"
							  ++ showEM (n+1) i
							  ++ showEM (n+1) d

showEM n (Division i d) = (replicate (2*n) ' ') ++ "Division:\n"
					    ++ showEM (n+1) i
						  ++ showEM (n+1) d

showEM n (Potencia i d) = (replicate (2*n) ' ') ++ "Potencia:\n"
					  ++ showEM (n+1) i
					  ++ showEM (n+1) d 

showEM n (Entero e) = (replicate (2*n) ' ') ++ "Entero: " ++ e ++ "\n"
showEM n (Real e) = (replicate (2*n) ' ') ++ "Real: " ++ e ++ "\n" 
showEM n (ConstMat e) = (replicate (2*n) ' ') ++ "Constante: " ++ e ++ "\n" 
showEM n (Variable v) = (replicate (2*n) ' ') ++ "Variable: " ++ v ++ "\n" 

showEM n (Funcion nombre cuerpo) = (replicate (2*n) ' ') ++ "Funcion "
									++ nombre ++ " (\n"
									++ showEM (n+1) cuerpo
									++ " )\n"

showEM n ( ArregloEM e ) = (replicate (2*n) ' ') ++ "Arreglo:\n"
							++ (replicate (2*n + 2) ' ') ++ "[\n"
							++ showArregloEM (n+1) e
							++ (replicate (2*n + 2) ' ') ++  "]\n"

showEM n (Rango inf sup) = (replicate (2*n) ' ') ++ "Rango:\n"
						  ++ showEM (n+1) inf
						  ++ showEM (n+1) sup

showEM n (ArregloComprension exp var arr) = (replicate (2*n) ' ') ++ "Arreglo Comprension:\n"
										   ++ (replicate (2*n + 1) ' ') ++ "Expresion:\n"
										   ++ showEM (n+1) exp
										   ++ (replicate (2*n + 1) ' ') ++ "Variable:\n"
										   ++ showEM (n+1) var
										   ++ (replicate (2*n + 1) ' ') ++ "ExpresionArreglo:\n"
										   ++ showEM (n+1) arr

showEM n (ExpresionCond cond r1 r2) = (replicate (2*n) ' ') ++ "Expresion Condicional:\n"
									 ++ showCond (n+1) cond
									 ++ showEM (n+1) r1
									 ++ showEM (n+1) r2

showArregloEM :: Int -> [EM] -> String
showArregloEM n [] = ""
showArregloEM n (e:es) = showEM n e
					    ++ showArregloEM n es 

showCond :: Int -> Condicional -> String
showCond n (Condicion em) = (replicate (2*n) ' ') ++ "Condicion:\n"
							++ showEM (n+1) em

showCond n (Conjuncion c1 c2) = (replicate (2*n) ' ') ++ "AND\n"
							   ++ showCond (n+1) c1
							   ++ showCond (n+1) c2

showCond n (Disyuncion i d) = (replicate (2*n) ' ') ++ "OR\n"
						    ++ showCond (n+1) i
						    ++ showCond (n+1) d

showCond n (Negacion c) = (replicate (2*n) ' ') ++ "NOT\n"
						   ++ showCond (n+1) c

showCond n (MayorQue i d) = (replicate (2*n) ' ') ++ ">\n"
						   ++ showEM (n+1) i
						   ++ showEM (n+1) d
						   
showCond n (MenorQue i d) = (replicate (2*n) ' ') ++ "<\n"
						   ++ showEM (n+1) i
						   ++ showEM (n+1) d

showCond n (MayorIgual i d) = (replicate (2*n) ' ') ++ ">=\n"
						     ++ showEM (n+1) i
							 ++ showEM (n+1) d

showCond n (MenorIgual i d) = (replicate (2*n) ' ') ++ "<=\n"
			 			     ++ showEM (n+1) i
							 ++ showEM (n+1) d

showCond n (Igual i d) = (replicate (2*n) ' ') ++ "==\n"
						++ showEM (n+1) i
						++ showEM (n+1) d
						   
showEG :: Int -> EG -> String
showEG n (Graficable em) = (replicate (2*n) ' ') ++ "Expresion Graficable:\n"
							++ showEM (n+1) em
showEG n (Archivo f) = (replicate (2*n) ' ') ++ "Archivo Graficable:\n"
						++ (replicate (2*n + 2) ' ') ++ f ++ "\n"

showBloque :: Int -> Bloque -> String
showBloque n (Secuencia []) = (replicate (2*n) ' ') ++ ""
showBloque n (Secuencia (x:xs)) = (replicate (2*n) ' ')
								++ show x ++ showBloque (n+1) (Secuencia xs) 

showInstruccion :: Int -> Instruccion -> String
showInstruccion n (DefFuncion name var cuerpo) = (replicate (2*n) ' ')
					 							++ "Declaracion\n"
												++ (replicate (2*n + 1) ' ')
												++ name ++ "( " ++ var ++ " )=\n"
												++ showEM (n+1) cuerpo
												++ "\n"

showInstruccion n (Asignacion v valor) = (replicate (2*n) ' ')
										++ "Asignacion " ++ v ++ " =\n"
										++ showEM (n+1) valor
										++ "\n"

showInstruccion n (Graficar e g ) = (replicate (2*n) ' ')
 									 ++ "Graficar\n"
									 ++ showEM (n+1) e
									 ++ showEG (n+1) g
									 ++ "\n"

showInstruccion n (GraficarEstilo e g estilos) = (replicate (2*n) ' ')
												 ++ "Graficar\n"
												 ++ showEM (n+1) e
												 ++ showEG (n+1) g
												 ++ "\n" ++ (replicate (2*n + 2) ' ')
												 ++ "with "
												 ++ show estilos
												 ++ "\n"
 
showInstruccion n (CicloStep var exp paso cuerpo) = (replicate (2*n) ' ')
												   ++ "Ciclo \n"
												   ++ (replicate (2*n + 1) ' ')
												   ++ "for " ++ var ++ " in \n"
												   ++ showEM (n+1) exp
												   ++ (replicate (2*n + 2) ' ')
												   ++ " step\n"
												   ++ showEM (n+1) paso
												   ++ showBloque (n+1) cuerpo
												   ++ "\n"

showInstruccion n (Ciclo var exp cuerpo) = (replicate (2*n) ' ')
										 ++ "Ciclo \n"
										 ++ (replicate (2*n + 1) ' ')					
										 ++ "for " ++ var ++ " in \n"
										 ++ showEM (n+1) exp
										 ++ showBloque (n+1) cuerpo
										 ++ "\n"
showInstruccion n (PushBack var exp) = (replicate (2*n) ' ')
								      ++ "push_back " ++ var
									  ++ showEM (n+1) exp
									  ++ "\n" 

instance Show Instruccion where
	  show fun = showInstruccion 0 fun
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
