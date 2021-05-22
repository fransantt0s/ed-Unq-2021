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



data Color = Azul | Rojo | Verde   deriving (Show,Eq)
data Celda = Bolita Color Celda | CeldaVacia deriving Show 



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

celda1 = (Bolita Rojo(Bolita Azul (Bolita Azul (Bolita Rojo CeldaVacia))))

poner:: Color -> Celda -> Celda 
poner c CeldaVacia = (Bolita c CeldaVacia)
poner Azul (Bolita color celda) = (Bolita color (Bolita Azul celda))
poner Rojo (Bolita color celda) = (Bolita color(Bolita Rojo celda))

sacar:: Color -> Celda -> Celda
sacar Azul (Bolita color celda) = if esAzul color
                                    then celda
                                    else sacar Azul celda


ponerN:: Int -> Color -> Celda -> Celda 
ponerN 0 c celda = celda 
ponerN n Azul celda = Bolita Azul (ponerNDeColor(n-1) Azul celda)
ponerN n Rojo celda = Bolita Rojo (ponerNDeColor(n-1) Rojo celda)
ponerN n Verde celda = Bolita Verde (ponerNDeColor(n-1) Verde celda)


ponerNDeColor:: Int -> Color -> Celda -> Celda
ponerNDeColor 0 _ celda = celda 
ponerNDeColor n c celda = Bolita c (ponerNDeColor (n-1) c celda)



data Objeto = Cacharro | Tesoro deriving (Show,Eq)
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show 

camino1 = Cofre [Cacharro] (Cofre [Cacharro,Tesoro] (Cofre [Tesoro] (Cofre [Tesoro] Fin)))

hayTesoro :: Camino -> Bool
hayTesoro Fin = False 
hayTesoro (Cofre obs camino) = elem Tesoro obs || hayTesoro camino
hayTesoro (Nada camino) = hayTesoro camino 


pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Cofre obs camino) = if elem Tesoro obs 
                                        then 0
                                        else 1 + pasosHastaTesoro camino
pasosHastaTesoro (Nada camino) = pasosHastaTesoro camino                                        


hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 Fin = False 
hayTesoroEn n Fin = False
hayTesoroEn 0 (Cofre obs camino) = elem Tesoro obs  
hayTesoroEn n (Cofre obs camino) = (hayTesoroEn(n-1) camino)
hayTesoroEn 0 (Nada camino) = False 
hayTesoroEn n (Nada camino) = hayTesoroEn (n-1) camino


cantTesoros:: Camino -> Int 
cantTesoros Fin = 0
cantTesoros (Cofre obs camino) = if elem Tesoro obs
                                    then 1 + cantTesoros camino 
                                    else cantTesoros camino 


apariciones :: Eq a => a -> [a] -> Int 
apariciones e [] = 0 
apariciones e (x:xs) = if x == e
                            then 1 + apariciones e xs 
                            else apariciones  e xs


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino =  (cantTesoros camino) >= n

unoSiPerteneceTesoro ::  obj -> [Objeto] 


cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 m Fin = 0 
cantTesorosEntre n m Fin = 0 
cantTesorosEntre 0 m (Cofre obs camino) = 
    apariciones Tesoro obs 
cantTesorosEntre n m (Cofre obs camino) =
    apariciones Tesoro obs  + cantTesorosAntesDe m camino
cantTesorosEntre 0 m (Nada camino) = 0     
cantTesorosEntre n m (Nada camino) = cantTersorosAntesDe (n-1) camino



cantTersorosAntesDe:: Int -> Camino 
cantTesorosAntesDe 0 Fin = 0 
cantTesorosAntesDe n Fin = 0 
cantTesorosAntesDe 0 (Cofre obs camino) = 
    apariciones Tesoro obs 
cantTesorosAntesDe n (Cofre obs camino) =
    apariciones Tesoro obs  + cantTesorosAntesDe(n-1)camino
cantTesorosAntesDe 0 (Nada camino) = 0     
cantTesorosAntesDe n (Nada camino) = cantTersorosAntesDe (n-1) camino