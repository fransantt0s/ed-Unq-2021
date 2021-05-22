-- Los ultimos elementos agregados son los primeros en salir---

module Stack(
    Stack,
    emptyS,
    isEmptyS,
    push,
    top,
    pop,
    lenS
)
where

data Stack a = S [a] deriving Show 

emptyS :: Stack a
emptyS = S []

isEmptyS :: Stack a -> Bool
isEmptyS (S ls) = null ls 

push :: a -> Stack a -> Stack a
push e (S ls) = S(ls ++ [e])

top :: Stack a -> a
top (S ls) = last(ls)

pop :: Stack a -> Stack a
pop (S ls) = S(init(ls))

lenS :: Stack a -> Int
lenS (S ls) = length(ls)
