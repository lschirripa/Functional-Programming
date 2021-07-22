module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-------------------- Guantelete --------------
data Guantelete = Guantelete {
material :: String,
gema :: [String]
}


-------------------- Personaje --------------
data Personaje = Personaje {
nombre :: String,
edad :: Number,
planeta :: String, 
energia :: Number,
habilidades :: [String]
}

-------------------- Universo --------------
data Universo = Universo {
personajes :: [Personaje]

}
