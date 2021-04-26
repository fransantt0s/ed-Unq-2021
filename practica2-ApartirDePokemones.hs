data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

pokemon1 = ConsPokemon Fuego 9
pokemon2 = ConsPokemon Agua 10 
pokemon3 = ConsPokemon Planta 8

entrenador1 = ConsEntrenador "Fran" [pokemon1,pokemon2,pokemon3,pokemon2]


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


