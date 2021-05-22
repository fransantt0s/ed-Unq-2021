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


agregarOtrosNDeColor:: Int -> Color -> Torre -> Torre
agregarOtrosNDeColor 0 _ torre = torre 
agregarOtrosN n c torre = Bloque c (agregarOtrosNDeColor (n-1) c torre)


agregarOtrosN:: Int -> Color -> Torre -> Torre
agregarOtrosN 0 color torre = torre
agregarOtrosN n c (Bloque color torre) =  if c == color
                                                then (Bloque color(Bloque color(agregarOtrosN(n-1) c torre)))
                                                else Bloque color (agregarOtrosN n c torre)                                          

torre1 = Bloque Rojo (Bloque Azul (Bloque Rojo (Bloque Verde (Base))))


aparicionesDeColores:: Torre -> [(Color,Int)]
aparicionesDeColores Base = []
aparicionesDeColores (Bloque color torre) = 
    agregarAparicion color (aparicionesDeColores torre)

agregarAparicion:: Color ->  [(Color,Int)] -> [(Color,Int)]
agregarAparicion c [] = [(c,1)]
agregarAparicion c (t:ts) =
    if fst t == c 
        then (c, snd t + 1) : ts 
        else t:(agregarAparicion c ts)





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
         sinRepetidos(clavesDeDispositivos dispositivo ++ todasLasClaves esc1 ++ todasLasClaves esc2 ++ todasLasClaves esc3)


sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

clavesDeDispositivos::  Dispositivo -> [Clave]
clavesDeDispositivos (Dispositivo _ (x:xs)) = agregarSiNoEsta x xs 

agregarSiNoEsta:: Clave -> [Clave] -> [Clave]
agregarSiNoEsta clave claves = if pertenece clave claves
                                then claves 
                                else clave : claves

dispositivo2 = Dispositivo 2 [10,10,30,40]

pertenece :: Eq a => a -> [a] -> Bool 
pertenece e [] = False 
pertenece e (x:xs) = x == e || pertenece e xs

                          
escenario3 = Punto dispositivo1 (Pared dispositivo2) (Acceso 20) (Acceso 30)

dispositivo1 = Dispositivo 10 [10,20,30,50,60]
escenario1 = (Punto dispositivo1 (Acceso 10) (Acceso 2) (Acceso 4))
escenario2 = (Pared dispositivo1)

sePuedeSalirCon :: Energia -> Escenario -> Bool
sePuedeSalirCon en es = sePuedeSalirCon' [] en es 


sePuedeSalirCon' :: [Clave] -> Energia -> Escenario -> Bool
sePuedeSalirCon' calves energia (Acceso clave) =
    elem clave claves && energia >=0
sePuedeSalirCon' claves energia (Pared dispositivo) = False 
sePuedeSalirCon' claves energia (Punto dispositivo ei ec ed) =
    let (claves', energia') = (extClave claves dispositivo) in 
        (sePuedeSalirCon' claves energia ei) || 
        (sePuedeSalirCon' claves energia ec) ||
        (sePuedeSalirCon' claves energia ed) ||
        (sePuedeSalirCon' claves' energia' ei) ||
        (sePuedeSalirCon' claves energia' ec) || 
        (sePuedeSalirCon' claves' energia' ed)  

                            
energiaDelDispositivo :: Dispositivo -> Energia
energiaDelDispositivo (Dispositivo energia _ ) = energia 


caminoGanador:: [Direccion] -> Escenario -> Bool 
caminoGanador _ (Acceso clave) = True
caminoGanador _ (Pared dispositivo) = False 
caminoGanador [] _ = False 
caminoGanador (d:ds) (Punto Dispositivo ei ec ed)
    caminoGanador ds (avanzar d ei ec ed)

avanzar :: Direccion -> Escenario -> Escenario -> Escenario -> Escenario
avanzar Izquierda (Punto dispositivo ei ec ed) = ei
avanzar Centro (Punto dispositivo ei ec ed ) = ec 
avanzar Derecha (Punto dispositivo ei ec ed ) = ed 

todosLoscaminos :: Escenario ->[[Direccion]]
todosLoscaminos (Acceso clave) = []
todosLoscaminos (Pared dispositivo) = []
todosLoscaminos (Punto dispositivo ei ec ed) = 
    agregar Izquierda(todosLoscaminos ei) ++
    agregar Centro(todosLoscaminos ec) ++ 
    agregar Derecha(todosLoscaminos ed) ++ 

agregar :: Direccion -> [[Direccion]] -> [[Direccion]]
agregar d [] = [[d]]
agregar d (ds:dss) = (d:ds) : (agregar d dss) 

agregar' :: Direccion -> [[Direccion]] -> [[Direccion]]
agregar' d [] = []
agregar' d dss = (d:ds) : (agregar' d dss) 






-- todosLoscaminos :: Escenario ->[[Direccion]]
-- todosLoscaminos (Acceso clave) = [[]]
-- todosLoscaminos (Pared dispositivo) = [[]]
-- todosLoscaminos (Punto dispositivo ei ec ed) = 
--     agregar Izquierda(todosLoscaminos ei) ++
--     agregar Centro(todosLoscaminos ec) ++ 
--     agregar Derecha(todosLoscaminos ed)  

-- agregar :: Direccion -> [[Direccion]] -> [[Direccion]]
-- agregar d [ds] = [d:ds]
-- agregar d (ds:dss) = (d:ds) : (agregar d dss) 





