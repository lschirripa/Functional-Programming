module Library where
import PdePreludat

type Nombre = String
type Edad = Number
type Felicidonios = Number
type Habilidades = [String]
type Suenio = Persona -> Persona
type Fuente = Persona -> Persona

data Persona = Persona {
  nombre       :: Nombre,
  edad         :: Edad,
  habilidades  :: Habilidades,
  felicidonios :: Felicidonios,
  suenios      :: [Suenio]
}


--instance Show Persona where
 --show  persona = "Nombre: " ++ nombre  persona ++ "\n" ++ "Edad: " ++ (show.edad  ) persona  ++ "\n" ++ "Habilidades: " ++ (show.habilidades) persona ++ "\n" ++ "Felicidonios: " ++ (show.felicidonios) persona

instance Show Persona where
  show  persona = "Nombre: " ++ nombre persona ++ "\n" ++ "Edad: " ++ (show.edad) persona ++ "\n" ++ "Suenios: " ++ (show.suenios) persona ++ "\n" ++ "Habilidades: " ++ (show.habilidades) persona ++ "\n" ++ "Felicidonios: " ++ (show.felicidonios) persona



fede :: Persona
fede = Persona {
  nombre = "Fede",
  edad = 24,
  felicidonios = 50,
  habilidades = ["Catar vinos","Aprobar TPs", "Programar"],
  suenios = [recibirseDe "Ing. en sistemas",recibirseDe "Fullstack",viaja ["Montevideo"],queTodoSigaIgual]
}


muyFeliz :: Persona -> Bool
muyFeliz = (>100) . felicidonios

moderadamenteFeliz :: Persona -> Bool
moderadamenteFeliz persona = felicidonios persona <= 100 && felicidonios persona > 50

-- Punto 1
cantidadDeSuenios :: Persona -> Number
cantidadDeSuenios  =  length . suenios

coeficienteDeSatisfaccion :: Persona -> Number
coeficienteDeSatisfaccion persona 
  | muyFeliz persona                = felicidonios persona * edad persona 
  | moderadamenteFeliz persona      = cantidadDeSuenios persona * felicidonios persona 
  | otherwise                       = felicidonios persona `div` 2
  
gradoDeAmbicionDeUnaPersona :: Persona -> Number
gradoDeAmbicionDeUnaPersona persona 
 | muyFeliz persona                  =  felicidonios persona * cantidadDeSuenios persona
 | moderadamenteFeliz persona        =  edad persona * cantidadDeSuenios persona 
 | otherwise                         =  ((*2).cantidadDeSuenios) persona


-- Punto 2
nombreLargo :: Persona -> Bool
nombreLargo  = ( >10) . length . nombre  

personaSuertuda :: Persona -> Bool
personaSuertuda  =  even . ( *3) . coeficienteDeSatisfaccion

nombreLindo :: Persona -> Bool
nombreLindo  =  ( == 'a').last.nombre


-- Punto 3
agregarHabilidad :: String -> Suenio
agregarHabilidad habilidad persona = persona{
  habilidades = ((++ [habilidad]). habilidades) persona 
}

sumarFelicidonios :: Number -> Suenio
sumarFelicidonios felicidoniosASumar persona = persona{
  felicidonios = felicidonios persona + felicidoniosASumar
}

sumarEdad :: Suenio
sumarEdad persona = persona{
  edad = ((+1). edad )persona
}



recibirseDe :: String -> Suenio
recibirseDe carrera =  agregarHabilidad carrera . sumarFelicidonios (((*1000) . length) carrera) 

viaja :: [String] -> Suenio
viaja ciudades = sumarEdad . sumarFelicidonios (((*100) . length) ciudades)

enamorarseDeOtraPersona :: Persona -> Suenio
enamorarseDeOtraPersona personaQueAma = sumarFelicidonios (felicidonios personaQueAma)

queTodoSigaIgual :: Suenio
queTodoSigaIgual = id

comboPerfecto :: Suenio
comboPerfecto = (sumarFelicidonios 100 ) . (viaja ["Berazategui","Paris"]) . (recibirseDe "medicina")

------------------------------------------------------- PARTE 2 -------------------------------------------------------------
--Punto 4a: Fuente minimalista. Le cumple el primer sueño a la persona, y lo quita de la lista de sueños de esa persona.
cumpleSuenio :: Persona -> Persona
cumpleSuenio persona = ((head.suenios) persona) persona

quitarUltimoSuenio :: Persona -> Persona
quitarUltimoSuenio  persona = persona {
  suenios = ((drop 1). suenios) persona
}

fuenteMinimalista :: Persona -> Persona
fuenteMinimalista persona = (quitarUltimoSuenio.cumpleSuenio) persona

--Punto 4b: Fuente copada. Le cumple todos los sueños a la persona. La persona debe quedar sin sueños.
-- ↓↓↓ Ejercicio fuenteCopada resuelto sin recursividad ↓↓↓
fuenteCopada :: Persona -> Persona
fuenteCopada persona = ((\persona -> persona {suenios = []}).(foldr ($) persona).suenios) persona
-- ↓↓↓ Ejercicio fuenteCopada resuelto sin recursividad ↓↓↓
--fuenteCopada persona = persona{
--  felicidonios = (sum.(map felicidonios).((flip map) (suenios persona))) ($ persona),
--  edad = ((foldl1 max).(map edad).((flip map) (suenios persona))) ($ persona),
--  habilidades = habilidades persona ++ (foldl1 (++).(map ((drop ((length.habilidades) persona)).habilidades)).((flip map) (suenios persona))) ($ persona),
--  suenios=[]
--}
-- ↓↓↓ Ejercicio fuenteCopada resuelto con recursividad ↓↓↓
--fuenteCopada persona 
--  | ((==0).length.suenios) persona  = persona
--  | otherwise                       = (fuenteCopada.fuenteMinimalista) persona

--Punto 4c: Fuente a pedido. Le cumple el enésimo sueǹo a una persona, pero no lo quita de la lista de sueños.
fuenteAPedido :: Number -> Persona -> Persona
fuenteAPedido suenioPedido persona = (((!! (suenioPedido-1)).suenios) persona) persona

--Punto 4d: Fuente sorda. No le cumple ningún sueño.
fuenteSorda :: Persona -> Persona
fuenteSorda persona = persona




--Punto 5: Fuente ganadora
type Criterio = Persona -> Persona -> Bool

masFelicidonios :: Criterio
masFelicidonios persona1 persona2 =  (felicidonios persona1) >= (felicidonios persona2)

menosFelicidonios :: Criterio
menosFelicidonios persona1 persona2 = (felicidonios persona1) <= (felicidonios persona2)

masHabilidades :: Criterio
masHabilidades persona1 persona2 = ((length.habilidades) persona1) >= ((length.habilidades) persona2)

fuenteGanadora :: Criterio -> [Fuente] -> Persona -> Fuente
fuenteGanadora criterio [] persona = error "Lista de fuentes vacía"
fuenteGanadora criterio [fuente] persona = fuente
fuenteGanadora criterio (primerFuente:segundaFuente:restoDeFuentes) persona
  | criterio (primerFuente persona) (segundaFuente persona) = fuenteGanadora criterio (primerFuente:restoDeFuentes) persona
  | otherwise = fuenteGanadora criterio (segundaFuente: restoDeFuentes) persona




--Ejercicio 6

--Sueños valiosos son aquellos que, al cumplirlos, la persona queda con más de 100 felicidonios
suenioValioso :: Persona -> [Suenio]
suenioValioso persona = (filter ((>100).felicidonios.($ persona)) .suenios) persona

--Sueño raro es el que deja a la persona con la misma cantidad de felicidonios tras cumplirlo
suenioRaro :: Persona -> Bool
suenioRaro persona = (any (((==).felicidonios)persona).map(felicidonios.($ persona)).suenios) persona

--Dada una lista de personas, poder conocer la felicidad total de ese
totalFelicidoniosDelGrupo :: [Persona] -> Number
totalFelicidoniosDelGrupo  = foldr ((+).felicidonios.fuenteCopada) 0 




--Ejercicio 7
soniador :: Persona
soniador = Persona {
  nombre = "Ricky Fort",
  edad = 39,
  felicidonios = 50,
  habilidades = ["Hacer que los chicos basten","Manejar el rating y un rolls royce"],
  suenios = repeat queTodoSigaIgual
}
--Fuente sorda satisface a la persona soniador con infinitos sueños
-- “a esta persona Soniador con infinitos sueños queTodoSigaIgual y la Fuente fuenteSorda la invoco en la consola y sus campos siguen iguales” 