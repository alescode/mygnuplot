module TablaSimbolos (module TablaSimbolos) where

import AS

type Simbolo = Maybe EM

type TablaDeSimbolos = [ (String, Simbolo) ]

pertenece :: String -> TablaDeSimbolos -> Bool
pertenece s []   = False
pertenece s (e:es) = if fst e == s then True else pertenece s es

buscar :: String -> TablaDeSimbolos -> Simbolo
buscar s []     = Nothing
buscar s (e:es) = if fst e == s then snd e else buscar s es

insertar :: String -> Simbolo -> TablaDeSimbolos -> TablaDeSimbolos
insertar str sim tabla = (str, sim) : tabla 

reemplazar :: String -> Simbolo -> TablaDeSimbolos -> TablaDeSimbolos
reemplazar str sim []     = []
reemplazar str sim (e:es) = if fst e == str then (str, sim) : es else reemplazar str sim es

-- Novich
type Symbol   = Maybe EM

type SymTable = [ (String, Symbol) ]

isMember :: String       -- ^ Símbolo a buscar en la Tabla de Símbolos.
						-> SymTable  -- ^ Tabla de Símbolos.
						-> Bool      -- ^ ¿El Símbolo está en la Tabla de Símbolos?
isMember s []    = False
isMember s (e:t) = if fst(e) == s then True else isMember s t

find :: String       -- ^ Símbolo a buscar en la Tabla de Símbolos.
				-> SymTable  -- ^ Tabla de Símbolos.
				-> Symbol    -- ^ Valor asociado al Símbolo, si existe.
find i []    = Nothing
find i (e:t) = if fst(e) == i then snd(e) else find i t

insert :: String      -- ^ Símbolo a insertar en la Tabla de Símbolos.
					-> Symbol   -- ^ Valor a asociar.
					-> SymTable -- ^ Tabla de Símbolos donde insertar.
					-> SymTable -- ^ Nueva Tabla de Símbolos después de la inserción.
insert i v t = if isMember i t then t else (i,v) : t

replace ::	String      -- ^ Símbolo cuyo valor se quiere modificar.
						-> Symbol   -- ^ Nuevo valor asociado al símbolo.
						-> SymTable -- ^ Tabla de Símbolos a modificar.
						-> SymTable -- ^ Tabla de Símbolos después de la modificación.
replace i v t = 
	if isMember i t then doReplace i v t [] else t
	where
		doReplace i v (e:t) x = if fst(e) == i
											 		  then x ++ (i,v) : t 
													  else doReplace i v t (x ++ [e])


