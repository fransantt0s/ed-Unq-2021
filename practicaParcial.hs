data Pizza = Prepizza
            | Capa Ingrediente Pizza  
                deriving Show 


data Ingrediente = Salsa 
                 | Queso 
                 | Jamon 
                 | Aceitunas Int  
                    deriving Show 

pizza1 = (Capa Queso 
            (Capa Jamon (Capa Jamon  Prepizza))
                )



cantidadCapas :: Pizza -> Int 
cantidadCapas Prepizza = 0 
cantidadCapas (Capa i pizza) = 1 + cantidadCapas pizza



{- armarPizza:: [Ingrediente] -> Pizza 
armarPizza [] = Prepizza 
armarPizza (x:xs) =
 -}

{- agregarIngrediente :: Pizza -> Ingrediente -> Pizza 
agregarIngrediente Prepizza ing  = Prepizza 
agregarIngrediente (Capa ing pizza) ingrediente = (Capa ing)   -}


sacarJamon:: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing pizza) = if esJamon ing 
                                then (Prepizza)
                                else (Capa ing pizza)


esJamon:: Ingrediente -> Bool 
esJamon Jamon = True 
esJamon _ = False


data Dir = Izq | Der deriving Show 
data Objeto = Tesoro | Chatarra 
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre 
            | Bifuracion Cofre Mapa Mapa

cofre1 = Cofre [Chatarra,Chatarra]
cofre2 = Cofre [Chatarra,Chatarra,Tesoro]
mapa1 =  (Bifuracion cofre1
            (Bifuracion cofre1 (Fin cofre1) (Fin cofre1))
            (Fin cofre2))         

objetosCofre:: Cofre -> [Objeto]
objetosCofre (Cofre xs) = xs


hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre []) = False 
hayTesoroEnCofre (Cofre (x:xs)) = esTesoro x || hayTesoroEnCofre (Cofre xs ) 


hayTesoro:: Mapa -> Bool
hayTesoro (Fin cofre) = hayTesoroEnCofre cofre
hayTesoro (Bifuracion cofre mapa1 mapa2) = hayTesoroEnCofre cofre  || hayTesoro mapa1 || hayTesoro mapa2 

esTesoro:: Objeto -> Bool
esTesoro Tesoro = True 
esTesoro Chatarra = False 

hayTesoroEn:: [Dir] -> Mapa -> Bool 
hayTesoroEn [] mapa1 = hayTesoro mapa1
hayTesoroEn (x:xs) 





