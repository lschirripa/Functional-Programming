module Library where
import PdePreludat

type Herramientas = Unidad -> Unidad 
type SistemasDeDefensa = Ciudad -> Ciudad

data Unidad = Unidad {
 tipoDeUnidad :: String,
 nivelDeAtaque :: Number,
 herramientas :: [Herramientas]
}deriving (Show)

data Ciudad = Ciudad {
 nivelDeDefensa :: Number,
 batallon :: [Unidad],
 sistemasDeDefensa :: [SistemasDeDefensa],
 nombreCiudad :: String
}deriving (Show)

--------------------------------------------------------------------------------------------------------
--                                           HERRAMIENTAS
--------------------------------------------------------------------------------------------------------

espada :: Herramientas
espada unidad = unidad {
  nivelDeAtaque = (+100) (nivelDeAtaque unidad)
}

hacha :: Herramientas
hacha unidad = unidad {
  nivelDeAtaque = (+200) (nivelDeAtaque unidad)
}

--------------------------------------------------------------------------------------------------------
--                                             UNIDADES
--------------------------------------------------------------------------------------------------------

arquero      = Unidad "arqueros" 190 [espada]
catapulta    = Unidad "Catapulta" 40 [espada]
caballeria   = Unidad "caballeria" 150 [espada,hacha,hacha]
guerrero     = Unidad "guerrero" 40 [espada]

--------------------------------------------------------------------------------------------------------
--                                             CIUDADES
--------------------------------------------------------------------------------------------------------

ciudad1 = Ciudad 10 [catapulta] [muralla 5] "shelbyville"

--------------------------------------------------------------------------------------------------------
--                                             PUNTO 1A
--------------------------------------------------------------------------------------------------------

esGrosa = ((>160).nivelDeAtaque)

unidadGrosa :: Ciudad -> [Unidad]
unidadGrosa ciudad = filter (esGrosa) (batallon ciudad)

--------------------------------------------------------------------------------------------------------
--                                             PUNTO 1B
--------------------------------------------------------------------------------------------------------

las3primeras ciudad = take 3 (batallon ciudad)

tieneAtaquePoderoso ciudad = foldr1 (&&) (map ((>100).nivelDeAtaque) (las3primeras ciudad))

--------------------------------------------------------------------------------------------------------
--                                             PUNTO 1C
--------------------------------------------------------------------------------------------------------

cuantoAportaHerramienta :: Unidad -> Number
cuantoAportaHerramienta unidad = 
    (nivelDeAtaque ( foldr ($) (unidad) (herramientas unidad) ) - nivelDeAtaque unidad)


--------------------------------------------------------------------------------------------------------
--                                             PUNTO 2
--------------------------------------------------------------------------------------------------------

cantHerramientas :: Unidad -> Bool
cantHerramientas = (((>5)).length.herramientas) 

esCaballeria :: Unidad -> Bool
esCaballeria  = (=="caballeria").tipoDeUnidad 

nivelAtaqueycuantoAporta :: Unidad -> Number
nivelAtaqueycuantoAporta unidad = (cuantoAportaHerramienta unidad) + (nivelDeAtaque unidad)

poderOfensivo :: Unidad -> Number
poderOfensivo unidad
 | cantHerramientas unidad   = 100 + nivelAtaqueycuantoAporta unidad
 | esCaballeria unidad       = 2* nivelAtaqueycuantoAporta unidad
 | otherwise                 = nivelAtaqueycuantoAporta unidad 

--------------------------------------------------------------------------------------------------------
--                                             PUNTO 3
--------------------------------------------------------------------------------------------------------

batallon1Sobrevive :: [Unidad] -> [Unidad] -> Bool

batallon1Sobrevive [] [] = error "pincharon todos"
batallon1Sobrevive batallon1 []  = True
batallon1Sobrevive [] batallon2 = False
batallon1Sobrevive (cabezaBatallon1:restoBatallon1) (cabezaBatallon2:restoBatallon2)
 | (poderOfensivo cabezaBatallon1) > (poderOfensivo cabezaBatallon2)  = batallon1Sobrevive (cabezaBatallon1:restoBatallon1) restoBatallon2
 | (poderOfensivo cabezaBatallon2) > (poderOfensivo cabezaBatallon1)  = batallon1Sobrevive  restoBatallon1 (cabezaBatallon2:restoBatallon2)
 | otherwise                                                          = batallon1Sobrevive restoBatallon1 restoBatallon2

--------------------------------------------------------------------------------------------------------
--                                             PUNTO 4
--------------------------------------------------------------------------------------------------------

sumarNivelDeDefensa defensios ciudad = ((+defensios).nivelDeDefensa) ciudad

muralla :: Number -> SistemasDeDefensa
muralla altura ciudad = ciudad {
  nivelDeDefensa = sumarNivelDeDefensa (altura*3) ciudad, 
  nombreCiudad = "la gran ciudad de " ++ (nombreCiudad ciudad)
}

torreDeVigilancia :: SistemasDeDefensa
torreDeVigilancia ciudad = ciudad {
  nivelDeDefensa = sumarNivelDeDefensa 40 ciudad
}


aumentarValorDeAtaqueDeUnaUnidad :: Number -> Unidad -> Unidad
aumentarValorDeAtaqueDeUnaUnidad potenciosParaCadaUnidad unidad = unidad{
  nivelDeAtaque = ((+potenciosParaCadaUnidad).nivelDeAtaque) unidad
}

centroDeEntremaniento potenciosParaCadaUnidad ciudad = ciudad {
 batallon = map (aumentarValorDeAtaqueDeUnaUnidad potenciosParaCadaUnidad ) (batallon ciudad),
 nivelDeDefensa    = sumarNivelDeDefensa 10 ciudad
}

instalarBancosEnPlazas ciudad = ciudad  

--------------------------------------------------------------------------------------------------------
--                                             PUNTO 5
--------------------------------------------------------------------------------------------------------

poderDefensivo ciudad = nivelDeDefensa (foldr ($) (ciudad) (sistemasDeDefensa ciudad))

persepolis = Ciudad {
 nombreCiudad = "persepolis",
 nivelDeDefensa = 10,
 batallon = [],
 sistemasDeDefensa = [muralla 5, centroDeEntremaniento 15, torreDeVigilancia]
}

--------------------------------------------------------------------------------------------------------
--                                             PUNTO 6
--------------------------------------------------------------------------------------------------------

poderTotalAtaqueDelBatallon :: [Unidad] -> Number
poderTotalAtaqueDelBatallon batallon = sum (map (poderOfensivo) batallon)

sobreviveCiudad ciudad batallonDeAtaque = 
  (batallon1Sobrevive  (batallon ciudad) batallonDeAtaque) || poderDefensivo ciudad > (poderTotalAtaqueDelBatallon batallonDeAtaque)

--------------------------------------------------------------------------------------------------------
--                                             PUNTO 7
-------------------------------------------------------------------------------------------------------- 

ciudadDeLosArqueros = Ciudad {
 nombreCiudad = "ArqueroLandia",
 nivelDeDefensa = 10,
 batallon = repeat arquero,
 sistemasDeDefensa =  [muralla 5, centroDeEntremaniento 15, torreDeVigilancia]
}

-- En caso de solicitar unidadGrosa a la ciudad "ciudadDeLosArqueros", no se podra evaluar cuales son, ya que
-- funcion que aplica es un filter, y nunca me va a filtrar una lista infinita si esta no termina mas.
-- Por otro lado, cuando se le solicita ataquePoderoso a "ciudadDeLosArqueros", devolvera True, ya que
-- me estara evaluando si las primeras 3 unidades tienen >100 de ataque, y esto es verdad ya que la ciudad
-- de arqueros tiene todos arqueros, y sus primeros 3 tienen 190 de ataque cada uno.
-- Haskell pudo evaluar esta ultima funcion ya que opera con el metodo lazy evaluation, la cual solo toma 
-- lo que necesita, no tiene necesidad de seguir evaluando la lista entera para simplemente evaluar
-- los primeros tres. Como en el filter SI necesitaba tener la lista entera para filtrar, Haskell no pudo hacerlo.
