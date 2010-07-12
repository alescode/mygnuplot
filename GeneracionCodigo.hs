module GeneracionCodigo (generarCodigo) where
import AS
import TablaSimbolos
import System.IO
import Debug.Trace
import qualified Data.Map as Map

generarCodigo :: Bloque -> TablaDeSimbolos -> [String]
generarCodigo (Secuencia lista) tabla = traducir tabla lista
    where 
          traducir tabla ((DefFuncion nombre (Variable var) expresion):ls) =  
              --trace (show $ Map.insert nombre (var, expresion) tabla)
                    traducir (Map.insert nombre (var, expresion) tabla) ls 
          traducir tabla ((Graficar rango expresion):ls) = 
              trace (show tabla)
              instruccionGraficar : traducir tabla ls
              where instruccionGraficar = "plot " ++ evaluarEM tabla expresion 
          traducir tabla (_:ls) = traducir tabla ls
          traducir tabla [] = []

evaluarEM :: TablaDeSimbolos -> EM -> String
evaluarEM tabla (Suma e1 e2) =
         concat ["(", evaluarEM tabla e1, ") + (", evaluarEM tabla e2, ")"]
evaluarEM tabla (Resta e1 e2) =
         concat ["(", evaluarEM tabla e1, ") - (", evaluarEM tabla e2, ")"]
evaluarEM tabla (Multiplicacion e1 e2) =
         concat ["(", evaluarEM tabla e1, ") * (", evaluarEM tabla e2, ")"]
evaluarEM tabla (Division e1 e2) =
         concat ["(", evaluarEM tabla e1, ") / (", evaluarEM tabla e2, ")"]
evaluarEM tabla (Potencia e1 e2) =
         concat ["(", evaluarEM tabla e1, ") ** (", evaluarEM tabla e2, ")"]
evaluarEM tabla (Menos e1) =
         concat ["-(", evaluarEM tabla e1, ")"]
evaluarEM tabla (Entero e1) = show e1
evaluarEM tabla (Real e1) = show e1
evaluarEM tabla (EMVariable (Variable s)) = "x"
evaluarEM tabla (EMLlamada (LlamadaFuncion nombre expresion)) =
        case Map.lookup nombre tabla of
              Nothing -> if nombre `elem` funcionesPredefinidas 
                         then concat [nombre, "(", evaluarEM tabla expresion, ")"] 
                         else error $ "error: no se encontro la funcion " ++ nombre
              Just e  -> evaluarEM tabla expresion -- desenrollar esta funcion!   

--traducir :: TablaDeSimbolos -> [Instruccion] -> [String]
--traducir tabla [] = ""
------ Definicion de funciones
--traducir tabla  ((DefFuncion nombre (Variable var) expresion):ls) = 
--    traducir (Map.insert nombre (var, expresion) tabla) ls

    --m <- Map.insert nombre (var, expresion) tabla -- se actualiza la tabla de simbolos
--traducir _ aSalida _ = ""
    
--traducir tabla _ (Asignacion _ em) = do
--    putStrLn $ evaluarEM tabla em 
--
--traducir tabla _ (Asignacion _ (EMLlamada (LlamadaFuncion nombre expresion))) = do
--    existe <- TablaSimbolos.lookup tabla nombre
--    case existe of
--         Nothing -> error $ "error: no existe la funcion " ++ nombre
--         Just _ -> return ()
---- Cualquier otro caso
--traducir _ aSalida _ = do
--                 --hPutStrLn aSalida "HOLA"
--                 return () -- no se genera codigo 

-- lv: lista de variables
--contarVariablesLibres :: LlamadaFuncion -> [String] -> Int
--contarVariablesLibres lv (LlamadaFuncion _ expresion) = cVL listaVar expresion
--    where cVL lv (Suma e1 e2) = cVL e1 + cVL e2
--          cVL lv (Resta e1 e2) = cVL e1 + cVL e2
--          cVL lv (Menos e1) = cVL e1
--          cVL lv (Multiplicacion e1 e2) = cVL e1 + cVL e2
--          cVL lv (Division e1 e2) = cVL e1 + cVL e2
--          cVL lv (Potencia e1 e2) = cVL e1 + cVL e2
--          cVL _ (Entero _) = 0
--          cVL _ (Real _) = 0
--          cVL f@(LlamadaFuncion _ _) = contarVariablesLibres f []
--          cVL lv EMVariable (Variable v) =  
--              case elemIndex v lv of
--                   Nothing -> 

--myReturn :: (Monad m) => m a -> a
--myReturn m a = a


--         case TablaSimbolos.lookup tabla nombre of
--              Nothing -> "a"
--              Just _ -> "b"
--         a <- return "hola2"
--         a
         --return a
         --a <- TablaSimbolos.lookup tabla nombre


    --case existe of
    --     Nothing -> error $ "error: no existe la funcion " ++ nombre
    --     Just _ -> return 'a'
--         show e1
--evaluarEM tabla (ArregloEM [EM] ) =
--         show e1

--producirEvaluacion :: LlamadaFuncion -> String
--producirEvaluacion (LlamadaFuncion f expresion) = cVL listaVar expresion
--    where cVL lv (Suma e1 e2) = cVL e1 + cVL e2
--          cVL lv (Resta e1 e2) = cVL e1 + cVL e2
--          cVL lv (Menos e1) = cVL e1
--          cVL lv (Multiplicacion e1 e2) = cVL e1 + cVL e2
--          cVL lv (Division e1 e2) = cVL e1 + cVL e2
--          cVL lv (Potencia e1 e2) = cVL e1 + cVL e2
--          cVL _ (Entero _) = 0
--          cVL _ (Real _) = 0
--          cVL f@(LlamadaFuncion _ _) = contarVariablesLibres f []
--          cVL lv EMVariable (Variable v) =  
--              case elemIndex v lv of
--                   Nothing -> 

