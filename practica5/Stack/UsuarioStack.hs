import Stack

-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs )

-- Dada una pila devuelve una lista sin alterar el orden de los elementos.

p1 = apilar [1,2,3,4]

desapilar :: Stack a -> [a]
desapilar p = if isEmptyS p
              then []
              else  desapilar(pop p) ++ [top p]


-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos n e pila = if n == 0 
                          then push e pila
                          else insertarEnPos (n-1) e (pop pila)


