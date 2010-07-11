module Prueba (EM, SecuenciaExpMat, Lista) where

data EM = Expresion EM
        | Mas EM EM
        | Menos EM EM
        | MenosUnario EM
        | Por EM EM
        | Entre EM EM
        | Elevado EM EM
        | Entero String
        | ConstMat String
        | Funcion String EM
        | Variable String
        | ArregloVacio
--        | SecuenciaExpMat
--        | Secuencia SecuenciaExpMat EM
--        | SecuenciaExpMat Secuencia EM
        deriving (Show)

-- data ArregloEM = ArregloVacio
--                | SecExpresionMat
--                deriving (Show)
-- 
-- data SecuenciaExpMat = EM -- Por que no funciona??

data SecuenciaExpMat = Unitaria EM
                     | Secuencia (SecuenciaExpMat) EM
                     deriving (Show)  

data Lista = Lvac
           | Cons (Lista) Int
           deriving (Show)
