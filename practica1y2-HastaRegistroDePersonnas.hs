 

    
    sucesor:: Int -> Int 
    sucesor x = x + 1

    suma:: Int -> Int -> Int 
    suma x y = x + y 

    divisionYResto :: Int -> Int -> (Int,Int)
    divisionYResto x y = ( div x y , mod x y ) 

    maxDelPar :: (Int,Int) -> Int 
    maxDelPar (x,y) = max x y 

    data  Dir = Norte | Sur | Este | Oeste deriving Show

    opuesto :: Dir -> Dir 
    opuesto Norte = Sur 
    opuesto Este = Oeste
    opuesto Oeste = Este 
    opuesto Sur = Norte 

    iguales :: Dir -> Dir -> Bool 
    iguales Norte Norte = True
    iguales Norte _ = False 
    iguales Este Este = True 
    iguales Este _ = False
    iguales Oeste Oeste = True
    iguales Oeste _ =  False
    iguales Sur Sur = True
    iguales Sur _ = False

    siguiente:: Dir -> Dir 
    siguiente Norte = Este 
    siguiente Este = Sur 
    siguiente Sur = Oeste 
    siguiente Oeste = Norte

    data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

    primeroYUltimo:: (DiaDeSemana,DiaDeSemana)
    primeroYUltimo = (Lunes,Viernes)

    empiezaConM :: DiaDeSemana -> Bool 
    empiezaConM Martes = True 
    empiezaConM Miercoles = True 
    empiezaConM _ = False

    vieneDespues :: DiaDeSemana -> DiaDeSemana ->  Bool
    vieneDespues Martes Lunes = True 
    vieneDespues Martes _  = False
    vieneDespues Miercoles Martes = True 
    vieneDespues Miercoles _ = False 
    vieneDespues Jueves Miercoles = True 
    vieneDespues Jueves _ = False 
    vieneDespues Viernes Jueves = True 
    vieneDespues Viernes _ = False
    vieneDespues Sabado Viernes = True 
    vieneDespues Sabado _ = False 
    vieneDespues Domingo Sabado = True 
    vieneDespues Domingo _ = False 
    vieneDespues Lunes Domingo = True  
    vieneDespues Lunes _ = False 

    estaEnElMedio :: DiaDeSemana -> Bool
    estaEnElMedio Lunes = False 
    estaEnElMedio Domingo = False 
    estaEnElMedio _ = True

    negar :: Bool -> Bool
    negar True = False
    negar False = True

    implica :: Bool -> Bool -> Bool
    implica True False = False 
    implica _ _ = True  

    and :: Bool -> Bool -> Bool
    and True True = True 
    and _ _ = False

    or:: Bool -> Bool -> Bool
    or True _ = True 
    or _ True = True 
    or _ _ = False


    data Persona = P String Int deriving Show

    persona1 = P "Jorge" 32
    persona2 = P "Franco" 22    

    nombre :: Persona -> String 
    nombre (P n e  )  = n 

    edad:: Persona -> Int 
    edad (P n e ) = e


   {-  crecer :: Persona -> Persona 
    crecer (P n e ) = (P n e+1) -}

    cambioDeNombre :: String -> Persona -> Persona 
    cambioDeNombre nombre (P n e) = (P nombre e)

    esMayorQueLaOtra :: Persona -> Persona -> Bool 
    esMayorQueLaOtra p1 p2 = edad p1 > edad p2

    laQueEsMayor:: Persona -> Persona -> Persona 
    laQueEsMayor p1 p2 = if edad p1 > edad p2 
                          then p1
                          else p2  


    data TipoPokemon = Agua | Fuego | Planta deriving Show 

    data Pokemon = Poke TipoPokemon Int deriving Show 

    pokemon1 = Poke Agua 20
    pokemon2 = Poke Fuego 80
    pokemon3 = Poke Planta 50 


    data Entrenador = E String Pokemon Pokemon deriving Show

    entrenador1 = E "Fran" pokemon1 pokemon1
    entrenador2 = E "Juan " pokemon1 pokemon2

    superaA :: Pokemon -> Pokemon -> Bool
    superaA (Poke Agua _ ) (Poke Fuego _) = True
    superaA (Poke Fuego _ ) (Poke Planta _ ) = True
    superaA (Poke Planta _ ) (Poke Agua _ )  = True 
    superaA (Poke _ _ ) (Poke _ _ ) = False

    cantidadDePokemonesDe :: TipoPokemon -> Entrenador -> Int 
    cantidadDePokemonesDe Agua (E _ (Poke Agua _) (Poke Agua _ )) = 2 
    cantidadDePokemonesDe Agua (E _ (Poke Agua _ ) (Poke _ _)) = 1
    cantidadDePokemonesDe Agua (E _ (Poke _ _ ) (Poke Agua _)) = 1
    cantidadDePokemonesDe Agua (E _ (Poke _ _ ) (Poke _ _)) = 0
    cantidadDePokemonesDe Fuego (E _ (Poke Fuego _) (Poke Fuego _ )) = 2 
    cantidadDePokemonesDe Fuego (E _ (Poke Fuego _ ) (Poke _ _)) = 1
    cantidadDePokemonesDe Fuego (E _ (Poke _ _ ) (Poke Fuego _)) = 1
    cantidadDePokemonesDe Fuego (E _ (Poke _ _ ) (Poke _ _)) = 0
    cantidadDePokemonesDe Planta (E _ (Poke Planta _) (Poke Planta _ )) = 2 
    cantidadDePokemonesDe Planta (E _ (Poke Planta _ ) (Poke _ _)) = 1
    cantidadDePokemonesDe Planta (E _ (Poke _ _ ) (Poke Planta _)) = 1
    cantidadDePokemonesDe Planta (E _ (Poke _ _ ) (Poke _ _)) = 0

    agarrarPokemones :: Entrenador -> [Pokemon]
    agarrarPokemones (E _ poke1 poke2) = poke1 : [poke2] 

    juntarPokemones:: (Entrenador,Entrenador) -> [Pokemon]
    juntarPokemones (e1,e2) = agarrarPokemones e1 ++ agarrarPokemones e2


    

    
------ Funciones Polimorficas --------------------------------

    loMismo :: a -> a 
    loMismo e1 = e1

    siempreSiete :: a -> Int 
    siempreSiete e1 = 7

    swap :: (a,b) -> (b,a)
    swap (x1,x2) = (x2,x1)

    estaVacia :: [a] -> Bool 
    estaVacia [] = True
    estaVacia ls = False

    elPrimero :: [a] -> a 
    elPrimero (x:xs) = x 

    sinElPrimero :: [a] -> [a]
    sinElPrimero [] = []
    sinElPrimero (x:xs) = xs

    splitHead :: [a] -> (a,[a])
    splitHead (x:[]) = (x,[])
    splitHead (x:xs) = (x,xs)

    sumatoria :: [Int] -> Int 
    sumatoria [] = 0
    sumatoria (x:xs) = x + sumatoria xs
    
    longitud :: [a] -> Int 
    longitud [] = 0
    longitud (x:xs) = 1 + longitud xs

    sucesores :: [Int] -> [Int]
    sucesores [] = []
    sucesores (x:xs) = x+1 : sucesores xs

    conjuncion :: [Bool] -> Bool 
    conjuncion [] = True 
    conjuncion(x:xs) = x == True && conjuncion xs

    disyuncion :: [Bool] -> Bool 
    disyuncion [] = False 
    disyuncion (x:xs) = x == True || disyuncion xs   

    productoria :: [Int] -> Int 
    productoria [] = 1 
    productoria (x:xs) = x * productoria xs

    algunoTrue :: [Bool] -> Bool 
    algunoTrue [] = False 
    algunoTrue (b:bs) = b || algunoTrue bs 

    porDos :: [Int] -> [Int]
    porDos [] = []
    porDos (x:xs) = x*2 : porDos xs 

    hayAlMenosUn5 :: [Int] -> Bool
    hayAlMenosUn5 [] = False
    hayAlMenosUn5 (x:xs) =  x == 5 || hayAlMenosUn5 xs
    
    hayAlMenosUnN :: Int -> [Int] -> Bool
    hayAlMenosUnN n [] = False
    hayAlMenosUnN n (x:xs) = x==n || hayAlMenosUnN n  xs 

    soloLosMayoresA :: Int -> [Int] -> [Int]
    soloLosMayoresA n [] = []
    soloLosMayoresA n (x:xs) = 
        if x>n
            then x : soloLosMayoresA n xs 
            else soloLosMayoresA n xs

    aplanar :: [[a]] -> [a]
    aplanar [] = []
    aplanar (x:xs) = x ++ aplanar xs

    pertenece :: Eq a => a -> [a] -> Bool 
    pertenece e [] = False 
    pertenece e (x:xs) = x == e || pertenece e xs

    apariciones :: Eq a => a -> [a] -> Int 
    apariciones e [] = 0 
    apariciones e (x:xs) = if x == e
                            then 1 + apariciones e xs 
                            else apariciones  e xs

    losMenoresA :: Int -> [Int] -> [Int]
    losMenoresA n [] = []
    losMenoresA n (x:xs) = if x<n
                            then x: losMenoresA n xs 
                            else losMenoresA n xs

                                                

    conLongitudMayorA  :: Int -> [[a]] -> [[a]]
    conLongitudMayorA n [] = []
    conLongitudMayorA n (x:xs) = 
         if longitud x>n
             then x: conLongitudMayorA n xs 
             else conLongitudMayorA n xs 

    agregarAlFinal:: [a] -> a -> [a]
    agregarAlFinal [] e = [e]
    agregarAlFinal (ls) e  = ls ++ [e]

    concatenar:: [a] -> [a] -> [a]
    concatenar [] ls = [] ++ ls
    concatenar ls [] = [] ++ ls 
    concatenar lista1 lista2 = lista1 ++ lista2

    reversa :: [a] -> [a] 
    reversa [] = []
    reversa (x:xs)  =  reversa xs ++ (x:[])


    ---[1,2,3,4,5]
    ---[2,3,4,5,1]
    -- [3,4,5,2,1]
    -- [4,5,3,2,1]
    -- [5,4,3,2,1]

    losPrimeros :: Int -> [a] -> [a]
    losPrimeros 0 xs = []  
    losPrimeros n [] = [] 
    losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

    zipMaximos:: [Int] -> [Int] -> [Int]
    zipMaximos [] [] = []
    zipMaximos []ls = ls
    zipMaximos ls [] = ls 
    zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys 

    elMinimo:: Ord a => [a] -> a
    elMinimo [] = error "la lista esta vacia"
    elMinimo(xs) = minimum xs 

    factorial :: Int -> Int 
    factorial 0 = 1 
    factorial n = n * factorial (n-1)

    cuentaRegresiva :: Int -> [Int]
    cuentaRegresiva 0 = []
    cuentaRegresiva n = n : cuentaRegresiva(n-1)

    repetir :: Int -> a -> [a]
    repetir 0 e = []
    repetir n e = e : repetir (n-1) e 

    primeros :: Int -> [a] -> [a]
    primeros n [] = []
    primeros 0 ls = []
    primeros n (x:xs) = x : primeros(n-1) xs

    sinLosPrimeros :: Int -> [a] -> [a]
    sinLosPrimeros n [] = []
    sinLosPrimeros 0 ls = ls 
    sinLosPrimeros n xs =  sinLosPrimeros(n-1) (sinElPrimero xs)

    ------Registros ------------------------------
    mayoresA:: Int -> [Persona] -> [Persona]
    mayoresA 0 xs = xs 
    mayoresA n [] = []
    mayoresA n (x:xs) = if  edad x > n 
                         then x : mayoresA n xs 
                         else mayoresA n xs


    sumarEdades :: [Persona] -> Int 
    sumarEdades [] = 0
    sumarEdades (x:xs) = edad x + sumarEdades xs 

    promedioEdad :: [Persona] -> Int
    promedioEdad [] = 0
    promedioEdad xs =  div (sumarEdades xs) (length xs)

    edadesDeLasPersonas :: [Persona] -> [Int]
    edadesDeLasPersonas [] = []
    edadesDeLasPersonas (x:xs) = edad x : edadesDeLasPersonas xs 

    -- edadMasAlta ::[Persona] -> Int  
    -- edadMasAlta (x:xs) =  

    persona3 = P "Carlos" 69 
    persona4 = P "Carlos" 99 

    elMasViejo :: [Persona] -> Persona
    elMasViejo (x:[]) = x 
    elMasViejo (xs) = if edad (elPrimero xs) == maximum (edadesDeLasPersonas xs)
                        then elPrimero xs 
                        else elMasViejo (sinElPrimero xs)                        


    





    
