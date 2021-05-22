module Queue2(
    Queue,
    emptyQ,
    isEmptyQ,
    queue,
    firstQ,
    dequeue,
)
where

data Queue a = Q [a] deriving Show 

--Constante O(1)---
emptyQ :: Queue a 
emptyQ = Q []

-- Constante, solo se fija si hay al menos un elemento O(1)--
isEmptyQ:: Queue a -> Bool
isEmptyQ (Q ls) = null ls 

-- Constante , solo agrega al principio, no le interesa ir hasta el final de la lista O(1)
queue :: a -> Queue a -> Queue a 
queue e (Q ls) = (Q (e:ls))

-- Lineal, O(n) donde n es la cantidad de elementos de la lista. Necesita recorrer toda la lista para saber cual es el ultimo elemento
firstQ :: Queue a -> a 
firstQ (Q ls) =  if null ls 
                  then error "cola vacia"
                  else last ls

-- Lineal , O(n) donde n es la cantidad de elementos de la lista. Recorre toda la lista para quitar solo el ultimo elemento.
dequeue:: Queue a -> Queue a 
dequeue (Q ls) = if null ls 
                 then error "cola vacia"
                 else Q(init ls)


