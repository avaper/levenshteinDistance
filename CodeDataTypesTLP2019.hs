module CodeDataTypesTLP2019 where

-- Tipos de dato para el calculo de la tabla --

type Fila = [Int]
type Tabla = [Fila]

-- Tipos de transiciones --

data Transicion = Borrar Char Int| Insertar Char Int | Cambiar Char Int Char

-- Instanciación de Transicion en la clase Show

instance Show Transicion where
  show (Borrar c n) = "Borrar "++show(c)++" en la posicion "++show(n)++"\n"
  show (Insertar c n) = "Insertar "++show(c)++" en la posicion "++show(n)++"\n"
  show (Cambiar a n b) = "Cambiar "++show(a)++" en la posicion "++show(n)++" por "++show(b)++"\n"

-- Tipo Solucion y función para mostrar las soluciones --

type Solucion = [Transicion]

mostrarSoluciones :: [Solucion] -> IO ()
mostrarSoluciones ls = putStr (mS ls)
  where mS [] = ""
        mS (s:ss) = (concat (map show s))++"\n"++(mS ss)

-- Tipo Nodo para el Backtracking --

data Nodo = Nodo { orig :: String , i :: Int , dest :: String , j :: Int , tabla :: Tabla , solucion :: Solucion }
  deriving Show

-- Función genérica de Backtracking --

bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt esSol compleciones nodo
  | esSol nodo = [nodo]
  | otherwise = concat (map (bt esSol compleciones) (compleciones nodo))

