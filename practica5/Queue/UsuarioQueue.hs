import Queue2

--Implementacion1 : Los elementos deben encolarse por el
-- final de la lista y desencolarse por delante.

--Implementacion 2 : agrega por delante y quita por el final de la lista.

lengthQ :: Queue a -> Int 
lengthQ q = if  isEmptyQ q 
            then 0
            else 1 + lengthQ(dequeue q)


queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q 
                then []
                else queueToList(dequeue q) ++ [firstQ q]


q2= (queue 1(queue 2 emptyQ))

q1 = (queue 3(queue 4 emptyQ))

unionQ :: Queue a -> Queue a -> Queue a 
unionQ q1 q2 = if isEmptyQ q2
                then q1
                else unionQ (queue (firstQ q2) q1) (dequeue q2) 


