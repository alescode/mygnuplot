module AS (
      Variable(..),
      LlamadaFuncion(..),
      CLlamadaFuncion(..),
      EM(..),
      Condicional(..),
      Bloque(..),
      Instruccion(..),
      Estilo(..),
      readEstilo
) where

data Variable = Variable String
              deriving (Eq)

-- Tipo de datos que representa una llamada a una funcion,
-- una expresion matematica del estilo f(x)
data LlamadaFuncion = LlamadaFuncion String EM
                    deriving (Eq)

-- Tipo de datos (analogo al anterior) que representa una llamada a una funcion,
-- una condicion del estilo f(x)
data CLlamadaFuncion = CLlamadaFuncion String Condicional
                    deriving (Eq)

-- Tipo de datos para Expresion Matematica
data EM = Suma EM EM
        | Resta EM EM
        | Menos EM
        | Multiplicacion EM EM
        | Division EM EM
        | Potencia EM EM
        | Entero Integer
        | Real Double
        | EMLlamada LlamadaFuncion
        | EMVariable Variable
        | ArregloEM [EM]
        | Rango EM EM
        | ArregloComprension EM Variable EM
        | ExpresionCond Condicional EM EM
        deriving (Eq)

-- Tipo de datos para expresiones condicionales
-- Debe contener constructores analogos a los de
-- Expresion Matematica, y algunos adicionales
data Condicional = CSuma Condicional Condicional
                 | CResta Condicional Condicional
                 | CMenos Condicional
                 | CMultiplicacion Condicional Condicional
                 | CDivision Condicional Condicional
                 | CPotencia Condicional Condicional
                 | CEntero Integer
                 | CReal Double
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

-- Tipo de datos que almacena la secuencia normal de instrucciones
-- del programa, o de un ciclo (bloque de codigo)
data Bloque = Secuencia [Instruccion]
            deriving (Eq)

-- Tipo de datos para representar instrucciones
data Instruccion = DefFuncion String Variable EM
                 | Asignacion Variable EM
                 | Graficar EM EM
                 | GraficarEstilo EM EM [Estilo]
                 | CicloStep Variable EM EM Bloque
                 | Ciclo Variable EM Bloque
                 | PushBack Variable EM
                 deriving (Eq)

-- Tipo de datos para representar distintos estilos de graficacion
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

{- Esta funcion permite construir un estilo con el
- constructor apropiado a partir de la cadena de caracteres leida
-}
readEstilo :: String -> Estilo
readEstilo "lines" = Lineas
readEstilo "points" = Puntos
readEstilo "linespoints" = LineasPunteadas

{- A continuacion se presentan funciones
- para mostrar de forma legible el arbol sintactico
-}
indentar :: Int -> String
indentar n = concat $ replicate n "|   "

instance Show EM where
    show em = showEM 0 em

instance Show Bloque where
    show bloque = showBloque 0 bloque

instance Show Condicional where
    show cond = showCond 0 cond

showEM :: Int -> EM -> String
showEM n (Suma i d) = indentar n ++ "Suma\n"
                    ++ showEM (n+1) i
                    ++ showEM (n+1) d

showEM n (Resta i d) = (indentar n) ++ "Resta\n"
                      ++ showEM (n+1) i
                      ++ showEM (n+1) d

showEM n (Menos e) = (indentar n) ++ "Menos\n"
                    ++ showEM (n+1) e

showEM n (Multiplicacion i d) = (indentar n) ++ "Multiplicacion\n"
                              ++ showEM (n+1) i
                              ++ showEM (n+1) d

showEM n (Division i d) = (indentar n) ++ "Division\n"
                         ++ showEM (n+1) i
                         ++ showEM (n+1) d

showEM n (Potencia i d) = (indentar n) ++ "Potencia\n"
                         ++ showEM (n+1) i
                         ++ showEM (n+1) d 

showEM n (Entero e) = (indentar n) ++ "Entero " ++ show e ++ "\n"
showEM n (Real e) = (indentar n) ++ "Real " ++ show e ++ "\n" 
showEM n (EMVariable v) = (indentar n) ++ show v ++ "\n"

showEM n (EMLlamada (LlamadaFuncion nombre cuerpo)) = (indentar n) ++ "Funcion "
                                    ++ nombre ++ "\n"
                                    ++ showEM (n+1) cuerpo

showEM n ( ArregloEM e ) = (indentar n) ++ "Arreglo\n"
                          ++ showArregloEM (n+1) e

showEM n (Rango inf sup) = (indentar n) ++ "Rango\n"
                          ++ showEM (n+1) inf
                          ++ showEM (n+1) sup

showEM n (ArregloComprension exp var arr) = (indentar n) ++ "Arreglo Comprension\n"
                                           ++ (indentar (n+1)) ++ "Expresion\n"
                                           ++ showEM (n+1) exp
                                           ++ (indentar (n+1)) ++ "Variable\n"
                                           ++ showEM (n+1) (EMVariable var)
                                           ++ (indentar (n+1)) ++ "ExpresionArreglo\n"
                                           ++ showEM (n+1) arr

showEM n (ExpresionCond cond r1 r2) = (indentar n) ++ "Expresion Condicional\n"
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
showCond n (CSuma i d) = (indentar n) ++ "Suma\n"
                    ++ showCond (n+1) i
                    ++ showCond (n+1) d

showCond n (CResta i d) = (indentar n) ++ "Resta\n"
                      ++ showCond (n+1) i
                      ++ showCond (n+1) d

showCond n (CMenos e) = (indentar n) ++ "Menos\n"
                    ++ showCond (n+1) e

showCond n (CMultiplicacion i d) = (indentar n) ++ "Multiplicacion\n"
                              ++ showCond (n+1) i
                              ++ showCond (n+1) d

showCond n (CDivision i d) = (indentar n) ++ "Division\n"
                        ++ showCond (n+1) i
                          ++ showCond (n+1) d

showCond n (CPotencia i d) = (indentar n) ++ "Potencia\n"
                      ++ showCond (n+1) i
                      ++ showCond (n+1) d 

showCond n (CEntero e) = (indentar n) ++ "Entero " ++ show e ++ "\n"
showCond n (CReal e) = (indentar n) ++ "Real " ++ show e ++ "\n" 
showCond n (CondicionalVariable v) = (indentar n) ++ show v ++ "\n"

showCond n (CondicionalLlamada (CLlamadaFuncion nombre cuerpo)) = (indentar n) ++ "Funcion "
                                    ++ nombre ++ "\n"
                                    ++ showCond (n+1) cuerpo

showCond n (ArregloCondicional e ) = (indentar n) ++ "Arreglo\n"
                                    ++ showArregloCond (n+1) e

showCond n (CRango inf sup) = (indentar n) ++ "Rango\n"
                          ++ showCond (n+1) inf
                          ++ showCond (n+1) sup

showCond n (CArregloComprension exp var arr) = (indentar n) ++ "Arreglo Comprension\n"
                                           ++ (indentar (n+1)) ++ "Expresion\n"
                                           ++ showCond (n+1) exp
                                           ++ (indentar (n+1)) ++ "Variable\n"
                                           ++ showCond (n+1) (CondicionalVariable var)
                                           ++ (indentar (n+1)) ++ "ExpresionArreglo\n"
                                           ++ showCond (n+1) arr

showCond n (CExpresionCond cond r1 r2) = (indentar n) ++ "Expresion Condicional\n"
                                     ++ showCond (n+1) cond
                                     ++ showCond (n+1) r1
                                     ++ showCond (n+1) r2     
                                     
showCond n (Conjuncion c1 c2) = (indentar n) ++ "Conjuncion\n"
                               ++ showCond (n+1) c1
                               ++ showCond (n+1) c2

showCond n (Disyuncion i d) = (indentar n) ++ "Disyuncion\n"
                            ++ showCond (n+1) i
                            ++ showCond (n+1) d

showCond n (Negacion c) = (indentar n) ++ "Negacion\n"
                           ++ showCond (n+1) c

showCond n (MayorQue i d) = (indentar n) ++ "Mayor\n"
                           ++ showCond (n+1) i
                           ++ showCond (n+1) d
                           
showCond n (MenorQue i d) = (indentar n) ++ "Menor\n"
                           ++ showCond (n+1) i
                           ++ showCond (n+1) d

showCond n (MayorIgual i d) = (indentar n) ++ "MayorIgual\n"
                             ++ showCond (n+1) i
                             ++ showCond (n+1) d

showCond n (MenorIgual i d) = (indentar n) ++ "MenorIgual\n"
                              ++ showCond (n+1) i
                             ++ showCond (n+1) d

showCond n (Igual i d) = (indentar n) ++ "Igual\n"
                        ++ showCond (n+1) i
                        ++ showCond (n+1) d
                           
showBloque :: Int -> Bloque -> String
showBloque n (Secuencia []) = ""
showBloque n (Secuencia (x:xs)) = (indentar n) ++ "Instruccion\n"
                                ++ showInstruccion (n+1) x
                                ++ showBloque (n) (Secuencia xs) 

showInstruccion :: Int -> Instruccion -> String
showInstruccion n (DefFuncion name var cuerpo) = (indentar n)
                                                 ++ "Definicion\n"
                                                ++ (indentar (n+1))
                                                ++ name ++ "\n"
                                                ++ (indentar (n+1))
                                                ++ show var ++ "\n"
                                                ++ showEM (n+1) cuerpo

showInstruccion n (Asignacion v valor) = (indentar n)
                                        ++ "Asignacion\n" 
                                        ++ showEM (n+1) (EMVariable v)
                                        ++ showEM (n+1) valor

showInstruccion n (Graficar e g ) = (indentar n)
                                      ++ "Graficar\n"
                                     ++ showEM (n+1) e
                                     ++ showEM (n+1) g

showInstruccion n (GraficarEstilo e g estilos) = (indentar n)
                                                 ++ "Graficar\n"
                                                 ++ showEM (n+1) e
                                                 ++ showEM (n+1) g
                                                 ++ (indentar (n+1))
                                                 ++ "Estilos "
                                                 ++ show estilos
                                                 ++ "\n"
 
showInstruccion n (CicloStep var exp paso cuerpo) = (indentar n)
                                                   ++ "Ciclo\n"
                                                   ++ (indentar (n+1)) ++ show var
                                                   ++ (indentar (n+1)) ++ "In\n"
                                                   ++ showEM (n+2) exp
                                                   ++ (indentar (n+1))
                                                   ++ "Paso\n"
                                                   ++ showEM (n+2) paso
                                                   ++ showBloque (n+1) cuerpo

showInstruccion n (Ciclo var exp cuerpo) = (indentar n)
                                         ++ "Ciclo\n"
                                         ++ (indentar (n+1)) ++ show var ++ "\n"
                                         ++ (indentar (n+1)) ++ "In\n"
                                         ++ showEM (n+2) exp
                                         ++ showBloque (n+1) cuerpo

showInstruccion n (PushBack var exp) = (indentar n)
                                      ++ "PushBack\n"
                                      ++ (indentar (n+1))
                                      ++ show var ++ "\n"
                                      ++ showEM (n+1) exp

instance Show Instruccion where
      show fun = showInstruccion 0 fun

