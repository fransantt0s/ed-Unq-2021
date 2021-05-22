
module Stack2(
    Stack,
    emptyS,
    isEmptyS,
    push,
    top,
    pop,
    lenS
)
where

data Stack a = [a] 

emptyS :: Stack a
emptyS xs = length xs


isEmptyS :: Stack a -> Bool
isEmptyS ls = null ls 

push :: a -> Stack a -> Stack a
push e  ls = ls ++ [e]

top :: Stack a -> a
top  ls = last(ls)

pop :: Stack a -> Stack a
pop ls = init(ls)

lenS :: Stack a -> Int
lenS ls = length(ls)