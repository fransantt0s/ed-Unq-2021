module Queue(
    Queue,
    emptyQ,
    isEmptyQ,
    queue,
    firstQ,
    dequeue
)
where

data Queue a = Q [a] deriving Show 

--Esta es la implementación menos costosa.

-- Constante---
emptyQ :: Queue a 
emptyQ = Q []

--Constante O(1)
isEmptyQ:: Queue a -> Bool
isEmptyQ (Q ls) = null ls 

--Lineal-- porque va hasta el final de la lista y despúes lo agrega
queue:: a -> Queue a -> Queue a 
queue e (Q ls) = Q(ls ++ [e])


--Constante--- Utiliza Head que es O(1)
firstQ:: Queue a -> a 
firstQ (Q ls ) = if null ls 
                 then error "cola vacia"
                 else head(ls)


--Constante-- Utiliza tail que es O(1)
dequeue :: Queue a -> Queue a 
dequeue (Q ls) = if null ls 
                 then error "cola vacia"
                 else (Q(tail ls))



