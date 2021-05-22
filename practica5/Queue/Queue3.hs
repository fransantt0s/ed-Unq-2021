--Implementación con 2 colas 

module Queue3(
    Queue,
    emptyQ,
    isEmptyQ,
    queue,
    firstQ,
    dequeue
)
where

data Queue a = Q [a] [a] deriving Show 

---Si fs se encuentra vacía entonces la cola está vacía


emptyQ :: Queue a 
emptyQ = Q [] []


isEmptyQ:: Queue a -> Bool
isEmptyQ (Q fs bs ) = null fs 


queue:: a -> Queue a -> Queue a 
queue e (Q fs bs) = 



firstQ:: Queue a -> a 
firstQ (Q fs bs ) = if null fs 
                 then error "cola vacia"
                 else head(fs)


dequeue :: Queue a -> Queue a 
dequeue (Q fs bs ) = if null fs 
                 then error "cola vacia"
                 else (Q(tail fs))
