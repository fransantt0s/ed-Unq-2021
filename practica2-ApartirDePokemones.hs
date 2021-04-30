data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

pokemon1 = ConsPokemon Fuego 9
pokemon2 = ConsPokemon Agua 10 
pokemon3 = ConsPokemon Planta 8

entrenador1 = ConsEntrenador "Fran" [pokemon1,pokemon2,pokemon3,pokemon2]
entrenador2 = ConsEntrenador "Juan" [pokemon2,pokemon3,pokemon1]
entrenador3 = ConsEntrenador "Jorge" [pokemon1,pokemon2]


cantPokemones :: Entrenador -> Int 
cantPokemones (ConsEntrenador _ []) = 0
cantPokemones(ConsEntrenador _ (pokemones)) = length pokemones

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (ConsPokemon tipo _) = tipo

unoSiEsMismoTipo:: TipoDePokemon -> Pokemon -> Int 
unoSiEsMismoTipo Agua (ConsPokemon Agua _ ) = 1
unoSiEsMismoTipo Agua (ConsPokemon _ _ ) = 0 
unoSiEsMismoTipo Fuego (ConsPokemon Fuego _ ) = 1
unoSiEsMismoTipo Fuego (ConsPokemon _ _ ) = 0 
unoSiEsMismoTipo Planta (ConsPokemon Planta _ ) = 1
unoSiEsMismoTipo Planta (ConsPokemon _ _ ) = 0 


pokemonesDeEntrenador :: Entrenador -> [Pokemon]
pokemonesDeEntrenador (ConsEntrenador _ pokes) = pokes 


cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe Agua (ConsEntrenador _ []) = 0 
cantPokemonesDe Agua (ConsEntrenador nombre (x:xs)) = unoSiEsMismoTipo Agua x + cantPokemonesDe Agua (ConsEntrenador nombre xs)
cantPokemonesDe Fuego (ConsEntrenador _ []) = 0 
cantPokemonesDe Fuego (ConsEntrenador nombre (x:xs)) = unoSiEsMismoTipo Fuego x + cantPokemonesDe Fuego (ConsEntrenador nombre xs)
cantPokemonesDe Planta (ConsEntrenador _ []) = 0 
cantPokemonesDe Planta (ConsEntrenador nombre (x:xs)) = unoSiEsMismoTipo Planta x + cantPokemonesDe Planta (ConsEntrenador nombre xs)

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon Agua _ ) (ConsPokemon Fuego _) = True
superaA (ConsPokemon Fuego _ ) (ConsPokemon Planta _ ) = True
superaA (ConsPokemon Planta _ ) (ConsPokemon Agua _ )  = True 
superaA (ConsPokemon _ _ ) (ConsPokemon _ _ ) = False

unoSiSuperaA :: [Pokemon ]-> [Pokemon ]-> Int
unoSiSuperaA [] [] = 0
unoSiSuperaA xs [] = 1
unoSiSuperaA (x:xs) (y:ys)= if superaA x y
                             then 1 + unoSiSuperaA xs ys
                             else unoSiSuperaA xs ys  


losQueLeGanan:: TipoDePokemon -> Entrenador -> Entrenador -> Int 
losQueLeGanan Agua (ConsEntrenador nombre pokemones1) (ConsEntrenador nombre2 pokemones2) = unoSiSuperaA pokemones1 pokemones2 + 
    losQueLeGanan Agua (ConsEntrenador nombre pokemones1) (ConsEntrenador nombre2 pokemones2)

esDeTipo:: TipoDePokemon -> Pokemon -> Bool
esDeTipo Agua   (ConsPokemon Agua _) = True
esDeTipo Agua   (ConsPokemon _ _ ) = False
esDeTipo Fuego  (ConsPokemon Fuego _ ) = True 
esDeTipo Fuego  (ConsPokemon _ _ ) = False
esDeTipo Planta (ConsPokemon Planta _) = True
esDeTipo Planta (ConsPokemon _ _ ) = False  


hayPokemonDeTipo:: TipoDePokemon -> [Pokemon] -> Bool
hayPokemonDeTipo Agua [] = False
hayPokemonDeTipo Agua (x:xs) = esDeTipo Agua x || hayPokemonDeTipo Agua xs
hayPokemonDeTipo Fuego (x:xs) = esDeTipo Fuego x || hayPokemonDeTipo Fuego xs
hayPokemonDeTipo Fuego [] = False 
hayPokemonDeTipo Planta (x:xs) = esDeTipo Planta x || hayPokemonDeTipo Planta xs
hayPokemonDeTipo Planta [] = False   

esMaestroPokemon:: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador nombre []) = False 
esMaestroPokemon (ConsEntrenador nombre pokes) = hayPokemonDeTipo Agua pokes && hayPokemonDeTipo Fuego pokes && hayPokemonDeTipo Planta pokes


data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

pertenece :: Eq a => a -> [a] -> Bool 
pertenece e [] = False 
pertenece e (x:xs) = x == e || pertenece e xs


nombre:: Proyecto -> String 
nombre (ConsProyecto n ) = n                       

proyectosDeRol:: Rol -> Proyecto
proyectosDeRol (Developer _ proyecto) = proyecto
proyectosDeRol (Management _ proyecto) = proyecto 


proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles[] = []
proyectosDeRoles(x:xs) = if esProyectoRepetido (proyectosDeRol x) (proyectosDeRoles xs )
                                    then proyectosDeRoles xs 
                                    else proyectosDeRol x : proyectosDeRoles xs 



esProyectoRepetido:: Proyecto -> [Proyecto] -> Bool 
esProyectoRepetido p [] = False 
esProyectoRepetido p (x:xs) = nombre p == nombre x || esProyectoRepetido p xs 




proyectos:: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa roles) = proyectosDeRoles roles




esDevSenior:: Rol -> Bool 
esDevSenior (Developer Senior _ ) = True 
esDevSenior (Developer _ _) = False
esDevSenior ( Developer _ _) = False 
esDevSenior (Management _ _) = False

esDevDelProyecto:: Rol -> Proyecto -> Bool 
esDevDelProyecto(Management _ _) p = False
esDevDelProyecto(Developer _ proyecto) p = nombre proyecto == nombre p 


{- data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol] -}

proyecto1 = ConsProyecto "app"
proyecto2 = ConsProyecto "jaskajk"
rol1 = Developer Senior proyecto1
rol2 = Developer Junior proyecto1
rol3 = Management Senior proyecto1
rol4 = Developer Senior proyecto1
rol5 = Developer Senior proyecto2 

empresa1 = ConsEmpresa [rol1,rol2,rol3,rol4,rol5]


losDevSenior::Empresa -> Int
losDevSenior (ConsEmpresa []) = 0
losDevSenior(ConsEmpresa (x:xs) ) = if esDevDelProyecto x (proyectosDeRol x )  && esDevSenior x
                                     then  1+ losDevSenior (ConsEmpresa xs)
                                     else losDevSenior (ConsEmpresa xs )


trabajaEnProyecto:: Rol -> Proyecto -> Bool
trabajaEnProyecto (Developer _ p) p1  = nombre p == nombre p1
trabajaEnProyecto (Management _ p) p1 = nombre p ==  nombre p1                                        


cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] emp = 0 
cantQueTrabajanEn (x:xs) (ConsEmpresa (rol1:rols)) = if trabajaEnProyecto rol1 x
                                                        then 1 + cantQueTrabajanEn (x:xs) (ConsEmpresa rols)
                                                        else cantQueTrabajanEn (x:xs) (ConsEmpresa rols ) 







    



