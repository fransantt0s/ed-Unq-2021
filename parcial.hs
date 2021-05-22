data Color = Azul | Verde | Rojo deriving (Show,Eq)
data Torre = Base
    | Bloque Color Torre deriving Show 


cantidadDeBloques :: Torre -> Int
cantidadDeBloques Base = 0
cantidadDeBloques (Bloque color torre ) = 1 + cantidadDeBloques torre

todos :: Color -> Torre -> Bool 
todos  colorBuscado Base = True
todos  colorBuscado (Bloque color torre) =  colorBuscado == color && todos colorBuscado torre




sinColores:: [Color]  -> Torre -> Torre 
sinColores [] Base = Base 
sinColores cols Base = Base
sinColores [] (Bloque color torre) = (Bloque color torre) 
sinColores (cols) (Bloque color torre) = if pertenece color cols 
                                            then (sinColores cols torre)
                                            else (Bloque color (sinColores cols torre)) 


torre1 = Bloque Rojo (Bloque Azul (Bloque Rojo (Bloque Verde (Base))))


aparicionesDeColores:: Torre -> [(Color,Int)]
aparicionesDeColores Base = []
aparicionesDeColores (Bloque color torre) = 
    (color,apariciones color (coloresDeLaTorre torre)) : aparicionesDeColores torre 
                


coloresDeLaTorre:: Torre -> [Color]
coloresDeLaTorre Base = [] 
coloresDeLaTorre (Bloque color torre)  = color : coloresDeLaTorre torre 


apariciones :: Eq a => a -> [a] -> Int 
apariciones e [] = 0 
apariciones e (x:xs) = if x == e
                            then 1 + apariciones e xs 
                            else apariciones  e xs



type Energia = Int
type Clave = Int
data Direccion = Izquierda | Centro | Derecha deriving (Show,Eq)
data Dispositivo = Dispositivo Energia [Clave] deriving Show 
data Escenario = Acceso Clave
    | Pared Dispositivo
    | Punto Dispositivo Escenario Escenario Escenario 
            deriving Show 


energia1 = 10
energia2  = 2 
energia3 = 4



cantidadDeAccesos :: Escenario -> Int 
cantidadDeAccesos (Acceso clave) = 1
cantidadDeAccesos (Pared dispositivo) = 0 
cantidadDeAccesos (Punto dispositivo esc1 esc2 esc3) = 0 + cantidadDeAccesos esc1 + cantidadDeAccesos esc2 + cantidadDeAccesos esc3 

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) ys
    | x `elem` ys = x : intersect xs ys
    | otherwise = intersect xs ys


todasLasClaves :: Escenario -> [Clave]
todasLasClaves (Acceso clave) = [clave]
todasLasClaves (Pared dispositivo) = clavesDeDispositivos dispositivo
todasLasClaves (Punto dispositivo esc1 esc2 esc3) = 
         clavesDeDispositivos dispositivo 
        ++ intersect (todasLasClaves esc1) (todasLasClaves esc2)  
        ++ intersect (todasLasClaves esc2) (todasLasClaves esc3)




dispositivo2 = Dispositivo 2 [10,10,30,40]


clavesDeDispositivos::  Dispositivo -> [Clave]
clavesDeDispositivos (Dispositivo _ (x:xs)) = agregarSiNoEsta x xs  

pertenece :: Eq a => a -> [a] -> Bool 
pertenece e [] = False 
pertenece e (x:xs) = x == e || pertenece e xs

agregarSiNoEsta:: Clave -> [Clave] -> [Clave]
agregarSiNoEsta clave claves = if pertenece clave claves
                                then claves 
                                else clave : claves
                           

escenario3 = Punto dispositivo1 (Pared dispositivo2) (Acceso 20) (Acceso 30)

dispositivo1 = Dispositivo 10 [10,20,30,50,60]
escenario1 = (Punto dispositivo1 (Acceso 10) (Acceso 2) (Acceso 4))
escenario2 = (Pared dispositivo1)

sePuedeSalirCon :: Energia -> Escenario -> Bool
sePuedeSalirCon energia (Acceso clave) = False 
sePuedeSalirCon energia (Pared dispositivo) =  energia >= energiaDelDispositivo dispositivo
sePuedeSalirCon energia (Punto dispositivo esc1 esc2 esc3) = 
    energia >= energiaDelDispositivo dispositivo || (sePuedeSalirCon energia esc1) || (sePuedeSalirCon energia esc2)
        || sePuedeSalirCon energia esc3

                            
energiaDelDispositivo :: Dispositivo -> Energia
energiaDelDispositivo (Dispositivo energia _ ) = energia 



