module GeneracionCodigo (procesar) where
import AS
import TablaSimbolos

procesar :: Bloque -> TablaDeSimbolos -> IO ()
procesar bloque tabla = do
                        obtenerVariables bloque tabla
                        return ()

obtenerVariables :: Bloque -> TablaDeSimbolos -> IO ()
obtenerVariables (Secuencia lista) tabla = do
                                           --return $ map (llenarTabla tabla) lista
                                           --llenarTabla tabla (lista !! 0)
                                           --insert tabla "hola" (Entero "2")
                                           llenarTabla tabla lista
                                           a <- toList tabla
                                           print a
                                           return ()

--llenarTabla :: TablaDeSimbolos -> Instruccion -> IO ()
--llenarTabla tabla (Asignacion (Variable nombre) expresion) = do
--        print "HOLA"
--        insert tabla nombre expresion
--        a <- toList tabla
--        return ()
--llenarTabla _ _ = do
--    print "BASURA"
--    return ()

llenarTabla :: TablaDeSimbolos -> [Instruccion] -> IO ()
llenarTabla _ [] = return ()
llenarTabla tabla ((Asignacion (Variable nombre) expresion):ins) = do
    update tabla nombre expresion
    llenarTabla tabla ins
llenarTabla _ _ = return ()

