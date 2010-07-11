module GeneracionCodigo (generarCodigo) where
import AS
import TablaSimbolos
import System.IO

generarCodigo :: Bloque -> TablaDeSimbolos -> Handle -> IO ()
generarCodigo (Secuencia lista) tabla aSalida = do
                                              --b <- return $ map (traducir tabla) lista
                                              mapM_ (traducir tabla aSalida) lista

traducir :: TablaDeSimbolos -> Handle -> Instruccion -> IO ()
-- Definicion de funciones
traducir tabla _ (DefFuncion nombre (Variable var) expresion) = do
    update tabla nombre (var, expresion) -- se actualiza la tabla de simbolos
    a <- toList tabla
    return ()
-- Cualquier otro caso
traducir _ aSalida _ = do
                 --hPutStrLn aSalida "HOLA"
                 return () -- no se genera codigo 

--traducir :: TablaDeSimbolos -> [Instruccion] -> IO ()
--traducir _ [] = return ()
--traducir tabla ((DefFuncion nombre (Variable var) expresion):ins) = do
--    update tabla nombre (var, expresion)
--    traducir tabla ins
--traducir tabla (_:ins) = traducir tabla ins

