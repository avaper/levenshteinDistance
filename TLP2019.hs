module TLP2019 where

import CodeDataTypesTLP2019

min3 a b c | (a <= b) && (a <= c) = a
           | (b <= a) && (b <= c) = b
           | otherwise            = c

-- -------------------------------------------------------------------------------------------------------------------------------------
-- FUNCIÓN creaSiguienteFila
-- -------------------------------------------------------------------------------------------------------------------------------------

creaSiguienteFila :: Char -> String -> Fila -> Fila
creaSiguienteFila caracter destino (f:fs) =  f+1: restoFila caracter destino (f+1) (f:fs)
    where
        restoFila :: Char -> [Char] -> Int -> [Int] -> [Int]
        restoFila c (d:ds) a [x,y]  | c == d    = [min3 (a+1) (y+1) (x+0)]  
                                    | otherwise = [min3 (a+1) (y+1) (x+1)]
        restoFila c (d:ds) a (f:fs) | c == d    = min3 (a+1) ((head fs)+1) (f+0): (restoFila c ds (min3 (a+1) ((head fs)+1) (f+0)) fs)
                                    | otherwise = min3 (a+1) ((head fs)+1) (f+1): (restoFila c ds (min3 (a+1) ((head fs)+1) (f+1)) fs)

-- -------------------------------------------------------------------------------------------------------------------------------------
-- FUNCIÓN creaTabla
-- -------------------------------------------------------------------------------------------------------------------------------------

creaTabla :: String -> String -> Tabla
creaTabla (o:os) (d:ds) = reverse ([0..(length (d:ds))]: restoTabla (o:os) (d:ds) [0..(length (d:ds))])
    where
        restoTabla :: [Char] -> [Char] -> Fila -> [Fila]
        restoTabla [] _ _ = []
        restoTabla (o:os) (d:ds) res = creaSiguienteFila o (d:ds) res: restoTabla os (d:ds) (creaSiguienteFila o (d:ds) res)

-- -- -------------------------------------------------------------------------------------------------------------------------------------
-- -- FUNCIÓN esSol
-- -- -------------------------------------------------------------------------------------------------------------------------------------

esSol :: Nodo -> Bool
esSol n | (0 == (i n)) && (0 == (j n)) = True
        | otherwise = False

-- -------------------------------------------------------------------------------------------------------------------------------------
-- FUNCIÓN compleciones
-- -------------------------------------------------------------------------------------------------------------------------------------

compleciones :: Nodo -> [Nodo]
compleciones nodo | esSol nodo = []
                  | otherwise = [head x|x<-nodosHijo nodo, (length x)>0]

nodosHijo n = [(if (insercion n) then [nodoIzquierda n] else []), (if (borrado n) then [nodoArriba n] else []), (if (cambio n) then [nodoDiagonal n] else [])]

insercion n = if ((j n) > 0) 
              then (valorNodo n == valorNodo (nodoIzquierda n) + 1) 
              else False
borrado n   = if ((i n) > 0) 
              then (valorNodo n == valorNodo (nodoArriba n) + 1)
              else False
cambio n    = if (((i n) > 0) && ((j n) > 0))
              then (if (head (orig n) /= head (dest n)) then (valorNodo n == valorNodo (nodoDiagonal n) + 1) else (valorNodo n == valorNodo (nodoDiagonal n)))
              else False

valorNodo n = [reverse x|x <- reverse (tabla n)]!!i n!!j n

nodoArriba n    = n {orig = tail (orig n), i = (i n) - 1, tabla = tail (tabla n), solucion = Borrar (head (orig n)) (i n)  : (solucion n)}
nodoIzquierda n = n {dest = tail (dest n), j = (j n) - 1, tabla = [tail x|x<- tabla n], solucion = Insertar (head (dest n)) (j n)  : (solucion n)}
nodoDiagonal n  = n {orig = tail (orig n), dest = tail (dest n), i = (i n) - 1, j = (j n) - 1, tabla = [tail x|x<-tail (tabla n)], solucion = (if (head (orig n) /= head (dest n)) then (Cambiar (head (orig n)) (j n) (head (dest n)) : (solucion n)) else (solucion n))}

-- -------------------------------------------------------------------------------------------------------------------------------------
-- FUNCIÓN nodoInicial
-- -------------------------------------------------------------------------------------------------------------------------------------

nodoInicial :: String -> String -> Nodo
nodoInicial origen destino = Nodo (reverse origen) (length (origen)) (reverse destino) (length (destino)) [reverse x | x <-(creaTabla origen destino)] []

-- -------------------------------------------------------------------------------------------------------------------------------------
-- FUNCIÓN obtenerTransiciones
-- -------------------------------------------------------------------------------------------------------------------------------------

obtenerTransiciones :: String -> String -> [Solucion]
obtenerTransiciones origen destino = [solucion x| x<-(bt esSol compleciones (nodoInicial origen destino))]

-- -------------------------------------------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------------------------------------------