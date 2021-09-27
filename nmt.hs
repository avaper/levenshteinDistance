nmt origen destino = nmtAux (reverse origen) (reverse destino)
  where nmtAux [] destino = length destino
        nmtAux origen [] = length origen
        nmtAux (o:os) (d:ds) = min3 b i c
          where b = 1 + (nmtAux os (d:ds))
                i = 1 + (nmtAux (o:os) ds)
                c
                  | o == d = nmtAux os ds
                  | otherwise = 1 + (nmtAux os ds)

min3 a b c
  | (a < b) && (a < c) = a
  | (b < a) && (b < c) = b
  | otherwise          = c
