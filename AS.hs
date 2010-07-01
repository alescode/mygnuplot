module AS (
      module AS
) where

data Variable = Variable String
              deriving (Eq)

data LlamadaFuncion = LlamadaFuncion String EM
                    deriving (Eq)

data CLlamadaFuncion = CLlamadaFuncion String Condicional
                    deriving (Eq)

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
showEM n (Suma i d) = (replicate (4*n) ' ') ++ "Suma\n"
					++ showEM (n+1) i
					++ showEM (n+1) d

showEM n (Resta i d) = (replicate (4*n) ' ') ++ "Resta\n"
					  ++ showEM (n+1) i
					  ++ showEM (n+1) d

showEM n (Menos e) = (replicate (4*n) ' ') ++ "Menos\n"
					++ showEM (n+1) e

showEM n (Multiplicacion i d) = (replicate (4*n) ' ') ++ "Multiplicacion\n"
							  ++ showEM (n+1) i
							  ++ showEM (n+1) d

showEM n (Division i d) = (replicate (4*n) ' ') ++ "Division\n"
					     ++ showEM (n+1) i
						 ++ showEM (n+1) d

showEM n (Potencia i d) = (replicate (4*n) ' ') ++ "Potencia\n"
						 ++ showEM (n+1) i
						 ++ showEM (n+1) d 

showEM n (Entero e) = (replicate (4*n) ' ') ++ "Entero " ++ e ++ "\n"
showEM n (Real e) = (replicate (4*n) ' ') ++ "Real " ++ e ++ "\n" 
showEM n (ConstMat e) = (replicate (4*n) ' ') ++ "Constante " ++ e ++ "\n" 
showEM n (EMVariable v) = (replicate (4*n) ' ') ++ show v ++ "\n"

showEM n (EMLlamada (LlamadaFuncion nombre cuerpo)) = (replicate (4*n) ' ') ++ "Funcion "
									++ nombre ++ "\n"
									++ showEM (n+1) cuerpo

showEM n ( ArregloEM e ) = (replicate (4*n) ' ') ++ "Arreglo\n"
						  ++ showArregloEM (n+1) e

showEM n (Rango inf sup) = (replicate (4*n) ' ') ++ "Rango\n"
						  ++ showEM (n+1) inf
						  ++ showEM (n+1) sup

showEM n (ArregloComprension exp var arr) = (replicate (4*n) ' ') ++ "Arreglo Comprension\n"
										   ++ (replicate (4*n + 1) ' ') ++ "Expresion\n"
										   ++ showEM (n+1) exp
										   ++ (replicate (4*n + 1) ' ') ++ "Variable\n"
										   ++ showEM (n+1) (EMVariable var)
										   ++ (replicate (4*n + 1) ' ') ++ "ExpresionArreglo\n"
										   ++ showEM (n+1) arr

showEM n (ExpresionCond cond r1 r2) = (replicate (4*n) ' ') ++ "Expresion Condicional\n"
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
showCond n (CSuma i d) = (replicate (4*n) ' ') ++ "Suma\n"
					++ showCond (n+1) i
					++ showCond (n+1) d

showCond n (CResta i d) = (replicate (4*n) ' ') ++ "Resta\n"
					  ++ showCond (n+1) i
					  ++ showCond (n+1) d

showCond n (CMenos e) = (replicate (4*n) ' ') ++ "Menos\n"
					++ showCond (n+1) e

showCond n (CMultiplicacion i d) = (replicate (4*n) ' ') ++ "Multiplicacion\n"
							  ++ showCond (n+1) i
							  ++ showCond (n+1) d

showCond n (CDivision i d) = (replicate (4*n) ' ') ++ "Division\n"
					    ++ showCond (n+1) i
						  ++ showCond (n+1) d

showCond n (CPotencia i d) = (replicate (4*n) ' ') ++ "Potencia\n"
					  ++ showCond (n+1) i
					  ++ showCond (n+1) d 

showCond n (CEntero e) = (replicate (4*n) ' ') ++ "Entero " ++ e ++ "\n"
showCond n (CReal e) = (replicate (4*n) ' ') ++ "Real " ++ e ++ "\n" 
showCond n (CConstMat e) = (replicate (4*n) ' ') ++ "Constante " ++ e ++ "\n" 
showCond n (CondicionalVariable v) = (replicate (4*n) ' ') ++ show v ++ "\n"

showCond n (CondicionalLlamada (CLlamadaFuncion nombre cuerpo)) = (replicate (4*n) ' ') ++ "Funcion "
									++ nombre ++ "\n"
									++ showCond (n+1) cuerpo

showCond n (ArregloCondicional e ) = (replicate (4*n) ' ') ++ "Arreglo\n"
									++ showArregloCond (n+1) e

showCond n (CRango inf sup) = (replicate (4*n) ' ') ++ "Rango\n"
						  ++ showCond (n+1) inf
						  ++ showCond (n+1) sup

showCond n (CArregloComprension exp var arr) = (replicate (4*n) ' ') ++ "Arreglo Comprension\n"
										   ++ (replicate (4*n + 1) ' ') ++ "Expresion\n"
										   ++ showCond (n+1) exp
										   ++ (replicate (4*n + 1) ' ') ++ "Variable\n"
										   ++ showCond (n+1) (CondicionalVariable var)
										   ++ (replicate (4*n + 1) ' ') ++ "ExpresionArreglo\n"
										   ++ showCond (n+1) arr

showCond n (CExpresionCond cond r1 r2) = (replicate (4*n) ' ') ++ "Expresion Condicional\n"
									 ++ showCond (n+1) cond
									 ++ showCond (n+1) r1
									 ++ showCond (n+1) r2     
                                     
showCond n (Conjuncion c1 c2) = (replicate (4*n) ' ') ++ "Conjuncion\n"
							   ++ showCond (n+1) c1
							   ++ showCond (n+1) c2

showCond n (Disyuncion i d) = (replicate (4*n) ' ') ++ "Disyuncion\n"
						    ++ showCond (n+1) i
						    ++ showCond (n+1) d

showCond n (Negacion c) = (replicate (4*n) ' ') ++ "Negacion\n"
						   ++ showCond (n+1) c

showCond n (MayorQue i d) = (replicate (4*n) ' ') ++ "Mayor\n"
						   ++ showCond (n+1) i
						   ++ showCond (n+1) d
						   
showCond n (MenorQue i d) = (replicate (4*n) ' ') ++ "Menor\n"
						   ++ showCond (n+1) i
						   ++ showCond (n+1) d

showCond n (MayorIgual i d) = (replicate (4*n) ' ') ++ "MayorIgual\n"
						     ++ showCond (n+1) i
							 ++ showCond (n+1) d

showCond n (MenorIgual i d) = (replicate (4*n) ' ') ++ "MenorIgual\n"
			 			     ++ showCond (n+1) i
							 ++ showCond (n+1) d

showCond n (Igual i d) = (replicate (4*n) ' ') ++ "Igual\n"
						++ showCond (n+1) i
						++ showCond (n+1) d
						   
showEG :: Int -> EG -> String
showEG n (Graficable em) = (replicate (4*n) ' ') ++ "Expresion Graficable\n"
							++ showEM (n+1) em
showEG n (Archivo f) = (replicate (4*n) ' ') ++ "Archivo Graficable\n"
						++ (replicate (4*n + 2) ' ') ++ f ++ "\n"

showBloque :: Int -> Bloque -> String
showBloque n (Secuencia []) = (replicate (4*n) ' ')
showBloque n (Secuencia (x:xs)) = (replicate (4*n) ' ') ++ "Secuencia\n"
								++ showInstruccion (n+1) x ++ showBloque (n) (Secuencia xs) 

showInstruccion :: Int -> Instruccion -> String
showInstruccion n (DefFuncion name var cuerpo) = (replicate (4*n) ' ')
					 							++ "Declaracion\n"
												++ (replicate (4*n + 1) ' ')
												++ name ++ "( " ++ show var ++ " )=\n"
												++ showEM (n+1) cuerpo

showInstruccion n (Asignacion v valor) = (replicate (4*n) ' ')
										++ "Asignacion\n" 
                                        ++ showEM (n+1) (EMVariable v)
										++ showEM (n+1) valor

showInstruccion n (Graficar e g ) = (replicate (4*n) ' ')
 									 ++ "Graficar\n"
									 ++ showEM (n+1) e
									 ++ showEG (n+1) g

showInstruccion n (GraficarEstilo e g estilos) = (replicate (4*n) ' ')
												 ++ "Graficar\n"
												 ++ showEM (n+1) e
												 ++ showEG (n+1) g
												 ++ "\n" ++ (replicate (4*n + 2) ' ')
												 ++ "Estilos "
												 ++ show estilos
												 ++ "\n"
 
showInstruccion n (CicloStep var exp paso cuerpo) = (replicate (4*n) ' ')
												   ++ "Ciclo \n"
												   ++ (replicate (4*n + 1) ' ')
												   ++ show var ++ "\n"
												   ++ (replicate (4*n + 1) ' ')
												   ++ "Rango " ++ "\n"
												   ++ showEM (n+1) exp
												   ++ (replicate (4*n + 2) ' ')
												   ++ " Paso\n"
											       ++ showEM (n+1) paso
												   ++ (replicate (4*n + 1) ' ')
												   ++ showBloque (n+1) cuerpo

showInstruccion n (Ciclo var exp cuerpo) = (replicate (4*n) ' ')
										 ++ "Ciclo \n"
										 ++ (replicate (4*n + 1) ' ')					
										 ++ show var ++ "\n"
										 ++ (replicate (4*n + 1) ' ')
										 ++ "Rango " ++ "\n"
										 ++ showEM (n+1) exp
										 ++ (replicate (4*n + 1) ' ')
										 ++ showBloque (n+1) cuerpo

showInstruccion n (PushBack var exp) = (replicate (4*n) ' ')
								      ++ "PushBack\n"
									  ++ (replicate (4*n + 2) ' ')
									  ++ show var ++ "\n"
									  ++ showEM (n+1) exp

instance Show Instruccion where
	  show fun = showInstruccion 0 fun

