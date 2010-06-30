module AS (
      module AS
) where

data Variable = Variable String
              deriving (Eq)

data LlamadaFuncion = LlamadaFuncion String EM
                    deriving (Eq, Show)

data CLlamadaFuncion = CLlamadaFuncion String Condicional
                    deriving (Eq, Show)

data EM = Suma EM EM
        | Resta EM EM
        | Menos EM
        | Multiplicacion EM EM
        | Division EM EM
        | Potencia EM EM
        | Entero String
        | Real String
        | ConstMat String
        | EMLlamada LlamadaFuncion
        | EMVariable Variable
        | ArregloEM [EM]
        | Rango EM EM
        | ArregloComprension EM Variable EM
        | ExpresionCond Condicional EM EM
        deriving (Eq)

data Condicional = CSuma Condicional Condicional
                 | CResta Condicional Condicional
                 | CMenos Condicional
                 | CMultiplicacion Condicional Condicional
                 | CDivision Condicional Condicional
                 | CPotencia Condicional Condicional
                 | CEntero String
                 | CReal String
                 | CConstMat String
                 | CondicionalLlamada CLlamadaFuncion
                 | CondicionalVariable Variable
                 | ArregloCondicional [Condicional]
                 | CRango Condicional Condicional
                 | CArregloComprension Condicional Variable Condicional
                 | CExpresionCond Condicional Condicional Condicional
                 | Conjuncion Condicional Condicional
                 | Disyuncion Condicional Condicional
                 | Negacion Condicional
                 | MayorQue Condicional Condicional
                 | MenorQue Condicional Condicional
                 | MayorIgual Condicional Condicional
                 | MenorIgual Condicional Condicional
                 | Igual Condicional Condicional
                 deriving (Eq)

data EG = Graficable EM
        | Archivo String
        deriving (Eq)

data Bloque = Secuencia [Instruccion]
            deriving (Eq)

data Instruccion = DefFuncion String Variable EM
                 | Asignacion Variable EM
				 | Graficar EM EG
                 | GraficarEstilo EM EG [Estilo]
                 | CicloStep Variable EM EM Bloque
                 | Ciclo Variable EM Bloque
                 | PushBack Variable EM
                 deriving (Eq)

data Estilo = Lineas
            | Puntos
            | LineasPunteadas 
            deriving (Eq)

instance Show Variable where
    show (Variable s) = "Variable " ++ s

instance Show Estilo where
    show Lineas = "Estilo lines"
    show Puntos = "Estilo points"
    show LineasPunteadas = "Estilo linespoints"

readEstilo :: String -> Estilo
readEstilo "lines" = Lineas
readEstilo "points" = Puntos
readEstilo "linespoints" = LineasPunteadas

instance Show EM where
	show em = showEM 0 em

instance Show EG where
	show e = showEG 0 e

instance Show Bloque where
    show bloque = showBloque 0 bloque

instance Show Condicional where
	show cond = showCond 0 cond

showEM :: Int -> EM -> String
showEM n (Suma i d) = (replicate (2*n) ' ') ++ "Suma\n"
					++ showEM (n+1) i
					++ showEM (n+1) d

showEM n (Resta i d) = (replicate (2*n) ' ') ++ "Resta\n"
					  ++ showEM (n+1) i
					  ++ showEM (n+1) d

showEM n (Menos e) = (replicate (2*n) ' ') ++ "Menos\n"
					++ showEM (n+1) e

showEM n (Multiplicacion i d) = (replicate (2*n) ' ') ++ "Multiplicacion\n"
							  ++ showEM (n+1) i
							  ++ showEM (n+1) d

showEM n (Division i d) = (replicate (2*n) ' ') ++ "Division\n"
					    ++ showEM (n+1) i
						  ++ showEM (n+1) d

showEM n (Potencia i d) = (replicate (2*n) ' ') ++ "Potencia\n"
					  ++ showEM (n+1) i
					  ++ showEM (n+1) d 

showEM n (Entero e) = (replicate (2*n) ' ') ++ "Entero " ++ e ++ "\n"
showEM n (Real e) = (replicate (2*n) ' ') ++ "Real " ++ e ++ "\n" 
showEM n (ConstMat e) = (replicate (2*n) ' ') ++ "Constante " ++ e ++ "\n" 
showEM n (EMVariable v) = (replicate (2*n) ' ') ++ show v

showEM n (EMLlamada (LlamadaFuncion nombre cuerpo)) = (replicate (2*n) ' ') ++ "Funcion "
									++ nombre ++ "\n"
									++ showEM (n+1) cuerpo

showEM n ( ArregloEM e ) = (replicate (2*n) ' ') ++ "Arreglo\n"
							++ (replicate (2*n + 2) ' ')
							++ showArregloEM (n+1) e
							++ (replicate (2*n + 2) ' ')

showEM n (Rango inf sup) = (replicate (2*n) ' ') ++ "Rango\n"
						  ++ showEM (n+1) inf
						  ++ showEM (n+1) sup

showEM n (ArregloComprension exp var arr) = (replicate (2*n) ' ') ++ "Arreglo Comprension\n"
										   ++ (replicate (2*n + 1) ' ') ++ "Expresion\n"
										   ++ showEM (n+1) exp
										   ++ (replicate (2*n + 1) ' ') ++ "Variable\n"
										   ++ showEM (n+1) (EMVariable var)
										   ++ (replicate (2*n + 1) ' ') ++ "ExpresionArreglo\n"
										   ++ showEM (n+1) arr

showEM n (ExpresionCond cond r1 r2) = (replicate (2*n) ' ') ++ "Expresion Condicional\n"
									 ++ showCond (n+1) cond
									 ++ showEM (n+1) r1
									 ++ showEM (n+1) r2

showArregloEM :: Int -> [EM] -> String
showArregloEM n [] = ""
showArregloEM n (e:es) = showEM n e
					    ++ showArregloEM n es 

showArregloCond :: Int -> [Condicional] -> String
showArregloCond n [] = ""
showArregloCond n (e:es) = showCond n e
					    ++ showArregloCond n es 

showCond :: Int -> Condicional -> String
showCond n (CSuma i d) = (replicate (2*n) ' ') ++ "Suma\n"
					++ showCond (n+1) i
					++ showCond (n+1) d

showCond n (CResta i d) = (replicate (2*n) ' ') ++ "Resta\n"
					  ++ showCond (n+1) i
					  ++ showCond (n+1) d

showCond n (CMenos e) = (replicate (2*n) ' ') ++ "Menos\n"
					++ showCond (n+1) e

showCond n (CMultiplicacion i d) = (replicate (2*n) ' ') ++ "Multiplicacion\n"
							  ++ showCond (n+1) i
							  ++ showCond (n+1) d

showCond n (CDivision i d) = (replicate (2*n) ' ') ++ "Division\n"
					    ++ showCond (n+1) i
						  ++ showCond (n+1) d

showCond n (CPotencia i d) = (replicate (2*n) ' ') ++ "Potencia\n"
					  ++ showCond (n+1) i
					  ++ showCond (n+1) d 

showCond n (CEntero e) = (replicate (2*n) ' ') ++ "Entero " ++ e ++ "\n"
showCond n (CReal e) = (replicate (2*n) ' ') ++ "Real " ++ e ++ "\n" 
showCond n (CConstMat e) = (replicate (2*n) ' ') ++ "Constante " ++ e ++ "\n" 
showCond n (CondicionalVariable v) = (replicate (2*n) ' ') ++ show v ++ "\n"

showCond n (CondicionalLlamada (CLlamadaFuncion nombre cuerpo)) = (replicate (2*n) ' ') ++ "Funcion "
									++ nombre ++ "\n"
									++ showCond (n+1) cuerpo

showCond n (ArregloCondicional e ) = (replicate (2*n) ' ') ++ "Arreglo\n"
							++ (replicate (2*n + 2) ' ')
							++ showArregloCond (n+1) e
							++ (replicate (2*n + 2) ' ')

showCond n (CRango inf sup) = (replicate (2*n) ' ') ++ "Rango\n"
						  ++ showCond (n+1) inf
						  ++ showCond (n+1) sup

showCond n (CArregloComprension exp var arr) = (replicate (2*n) ' ') ++ "Arreglo Comprension\n"
										   ++ (replicate (2*n + 1) ' ') ++ "Expresion\n"
										   ++ showCond (n+1) exp
										   ++ (replicate (2*n + 1) ' ') ++ "Variable\n"
										   ++ showCond (n+1) (CondicionalVariable var)
										   ++ (replicate (2*n + 1) ' ') ++ "ExpresionArreglo\n"
										   ++ showCond (n+1) arr

showCond n (CExpresionCond cond r1 r2) = (replicate (2*n) ' ') ++ "Expresion Condicional\n"
									 ++ showCond (n+1) cond
									 ++ showCond (n+1) r1
									 ++ showCond (n+1) r2     
                                     
showCond n (Conjuncion c1 c2) = (replicate (2*n) ' ') ++ "AND\n"
							   ++ showCond (n+1) c1
							   ++ showCond (n+1) c2

showCond n (Disyuncion i d) = (replicate (2*n) ' ') ++ "OR\n"
						    ++ showCond (n+1) i
						    ++ showCond (n+1) d

showCond n (Negacion c) = (replicate (2*n) ' ') ++ "NOT\n"
						   ++ showCond (n+1) c

showCond n (MayorQue i d) = (replicate (2*n) ' ') ++ ">\n"
						   ++ showCond (n+1) i
						   ++ showCond (n+1) d
						   
showCond n (MenorQue i d) = (replicate (2*n) ' ') ++ "<\n"
						   ++ showCond (n+1) i
						   ++ showCond (n+1) d

showCond n (MayorIgual i d) = (replicate (2*n) ' ') ++ ">=\n"
						     ++ showCond (n+1) i
							 ++ showCond (n+1) d

showCond n (MenorIgual i d) = (replicate (2*n) ' ') ++ "<=\n"
			 			     ++ showCond (n+1) i
							 ++ showCond (n+1) d

showCond n (Igual i d) = (replicate (2*n) ' ') ++ "==\n"
						++ showCond (n+1) i
						++ showCond (n+1) d
						   
showEG :: Int -> EG -> String
showEG n (Graficable em) = (replicate (2*n) ' ') ++ "Expresion Graficable\n"
							++ showEM (n+1) em
showEG n (Archivo f) = (replicate (2*n) ' ') ++ "Archivo Graficable\n"
						++ (replicate (2*n + 2) ' ') ++ f ++ "\n"

showBloque :: Int -> Bloque -> String
showBloque n (Secuencia []) = (replicate (2*n) ' ')
showBloque n (Secuencia (x:xs)) = (replicate (2*n) ' ')
								++ show x ++ showBloque (n+1) (Secuencia xs) 

showInstruccion :: Int -> Instruccion -> String
showInstruccion n (DefFuncion name var cuerpo) = (replicate (2*n) ' ')
					 							++ "Declaracion\n"
												++ (replicate (2*n + 1) ' ')
												++ name ++ "( " ++ show var ++ " )=\n"
												++ showEM (n+1) cuerpo
												++ "\n"

showInstruccion n (Asignacion v valor) = (replicate (2*n) ' ')
										++ "Asignacion\n" 
                                        ++ showEM (n+1) (EMVariable v) ++ "\n"
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
												   ++ "for " ++ show var ++ " in \n"
												   ++ showEM (n+1) exp
												   ++ (replicate (2*n + 2) ' ')
												   ++ " step\n"
												   ++ showEM (n+1) paso
												   ++ showBloque (n+1) cuerpo
												   ++ "\n"

showInstruccion n (Ciclo var exp cuerpo) = (replicate (2*n) ' ')
										 ++ "Ciclo \n"
										 ++ (replicate (2*n + 1) ' ')					
										 ++ "for " ++ show var ++ " in \n"
										 ++ showEM (n+1) exp
										 ++ showBloque (n+1) cuerpo
										 ++ "\n"
showInstruccion n (PushBack var exp) = (replicate (2*n) ' ')
								      ++ "push_back " ++ show var
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
