data Pizza = Prepizza
    | Capa Ingrediente Pizza deriving Show 

data Ingrediente = Salsa
    | Queso
    | Jamon
    | Aceitunas Int deriving Show 

p1 = Capa Jamon (Capa Queso (Capa Salsa (Capa Jamon(Capa Queso Prepizza))))
jamon = Capa Salsa (Capa Queso (Capa Jamon (Capa Queso (Capa Jamon Prepizza))))
aceitunas = Capa Salsa (Capa Queso (Capa (Aceitunas 2) Prepizza))




cantidadDecapas :: Pizza -> Int 
cantidadDecapas Prepizza = 0 
cantidadDecapas (Capa ing pizza) = 1 + cantidadDecapas pizza 


armarPizza:: [Ingrediente] -> Pizza 
armarPizza [] = Prepizza
armarPizza (x:xs) = (Capa  x (armarPizza xs))


sacarJamon:: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing pizza) =  if (esJamon ing)
                                then sacarJamon (pizza)
                                else (Capa ing (sacarJamon pizza)) 

esJamon:: Ingrediente  ->  Bool 
esJamon Jamon  = True 
esJamon Salsa  = False
esJamon Queso  = False 
esJamon (Aceitunas n) = False

esSalsa :: Ingrediente -> Bool 
esSalsa Salsa  = True 
esSalsa Queso  = False 
esSalsa Jamon = False
esSalsa (Aceitunas n)  = False  



esQueso :: Ingrediente -> Bool
esQueso Salsa  = False 
esQueso Queso  = True
esQueso Jamon  = False 
esQueso (Aceitunas n) = False

p2 = Capa Salsa(Capa Salsa Prepizza)


tieneSoloSalsa:: Pizza -> Bool 
tieneSoloSalsa Prepizza = False 
tieneSoloSalsa (Capa ing pizza) = if (esSalsa ing)
                                    then tieneSoloSalsa pizza
                                    else tieneSoloSalsa pizza  



data Color = Azul | Rojo  deriving Show 
data Celda = Bolita Color Celda | CeldaVacia deriving Show 


celda1 = (Bolita Rojo(Bolita Azul (Bolita Azul (Bolita Rojo CeldaVacia))))
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia = 0  
nroBolitas Rojo (Bolita color celda) = if esRojo color 
                                        then  1 + nroBolitas Rojo celda 
                                        else nroBolitas Rojo celda 
nroBolitas Azul (Bolita color celda) = if esAzul color
                                        then 1 + nroBolitas Azul celda
                                        else nroBolitas Azul celda                                         


esRojo:: Color -> Bool
esRojo Rojo = True
esRojo _ = False 

esAzul :: Color -> Bool
esAzul Azul = True 
esAzul _ = False 

celda2 = CeldaVacia


poner:: Color -> Celda -> Celda 
poner c CeldaVacia = (Bolita c CeldaVacia)
poner Azul (Bolita color celda) = (Bolita color (Bolita Azul celda))
poner Rojo (Bolita color celda) = (Bolita color(Bolita Rojo celda))


sacar:: Color -> Celda -> Celda
sacar Azul (Bolita color celda) = if esAzul color
                                    then celda
                                    else sacar Azul celda



ponerN:: Int -> Color -> Celda -> Celda 
ponerN 0 c CeldaVacia = CeldaVacia
ponerN n Azul CeldaVacia = poner Azul (ponerN  (n-1) Azul CeldaVacia)
ponerN n Azul (Bolita color celda) = poner Azul (ponerN (n-1) Azul (Bolita color celda))
