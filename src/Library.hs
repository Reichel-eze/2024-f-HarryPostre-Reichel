module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Harry Potter se puso un local de comidas especializadas en preparaciones dulces

-- postres --> se utilizan hechizos (los hechizos se usan sobre los postres para ir preparandolos)
--  (es decir que para hacer un postre necesito hechizos)

-- 1.POSTRES --

-- A) Modelar los postres

data Postre = UnPostre {
    nombre :: String,
    sabores :: [String],
    peso :: Number,         -- el peso en gramos
    temperatura :: Number   -- se sirve a determinada temperatura en °C
}deriving (Show,Eq)

bizcochoBorracho :: Postre
bizcochoBorracho = UnPostre "Bizcocho Borracho" ["fruta","crema"] 100 25

tartaDeMelaza :: Postre
tartaDeMelaza = UnPostre "Tarta de melaza" ["melaza"] 50 0 

-- Modificaciones en Campos --

cambioEnTemperatura :: Number -> Postre -> Postre
cambioEnTemperatura delta postre = postre {temperatura = temperatura postre + delta} 

bajaElPesoSegunPorcentaje :: Number -> Postre -> Postre
bajaElPesoSegunPorcentaje porcentaje postre = postre {peso = ((100 - porcentaje) * peso postre) `div` 100 }

congelaElPostre :: Postre -> Postre
congelaElPostre postre = postre {temperatura = 0} 

estaCongelado :: Postre -> Bool
estaCongelado =  (<=0).temperatura

agregarNuevoSabor :: String -> Postre -> Postre
agregarNuevoSabor newSabor postre = postre {sabores = newSabor : sabores postre}

eliminarTodosLosSabores :: Postre -> Postre
eliminarTodosLosSabores postre = postre {sabores = []}

listaNoEstaVacia :: [a] -> Bool  
listaNoEstaVacia = not . null

-- o sino mejor dicho

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor =  (>0) . length . sabores

-- B) Modelar los hechizos (se deben poder agregar mas SIN MODIFICAR EL CODIGO EXISTENTE!!)

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio  = cambioEnTemperatura 1 . bajaElPesoSegunPorcentaje 5 

immobulus :: Hechizo 
immobulus = congelaElPostre

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = bajaElPesoSegunPorcentaje 10 . agregarNuevoSabor "concentrado" 

diffindo :: Number -> Hechizo
diffindo porcentaje = bajaElPesoSegunPorcentaje porcentaje

riddikulus :: String -> Hechizo
riddikulus newNombre = agregarNuevoSabor (reverse newNombre)

avadakedavra :: Hechizo
avadakedavra = eliminarTodosLosSabores . congelaElPostre

-- C) Dado un conjunto de postres --> saber si un hechizo los dejaara listos A TODOS!! (all)
-- listos = un postre esta listo cuando pesa algo mas que cero, tiene algun sabor y ademas no esta congelado

dejaranListos :: [Postre] -> Hechizo -> Bool
dejaranListos postres hechizo = all (estaListo . hechizo) postres 

-- La expresión (estaListo . hechizo) es equivalente a \postre -> estaListo (hechizo postre)
-- Postre -> Postre . Postre -> Bool --------> Postre -> Bool
-- la funcion dentro del all "te dice que hace con cada uno de los elementos de la lista"

hechizoParaTodos :: [Postre] -> Hechizo -> [Postre]                   -- para poder ver los efectos del hechizo sobre una lista de postres
hechizoParaTodos postres hechizo = map (hacerHechizo hechizo) postres 

hacerHechizo :: Hechizo -> Postre -> Postre     -- para poder hacerle cualquier hechizo de los que hice al postre !! 
hacerHechizo hechizo postre = hechizo postre    -- (los hechizos que son con algun numbre por ej, los pongo entre () en la terminal)

--dejaListo :: Hechizo -> Postre -> Bool
--dejaListo hechizo postre = estaListo (hechizo postre)

estaListo :: Postre -> Bool
estaListo postre = (peso postre > 0) && (listaNoEstaVacia (sabores postre)) && (temperatura postre /= 0) 
--                                   && (tieneAlgunSabor postre)            && not (estaCongelado postre)  

-- D) Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos (OJO, tienen que ser los listos nomas)

pesoPromedio :: [Postre] -> Number                        -- 1ero) hago una lista de solo los postres listos
pesoPromedio = promedio . map peso . filter estaListo     -- 2dos) hago una lista con solo los pesos de esos postres listos
                                                          -- 3ero) calcuclo el promedio de esa lista de pesos    
promedio :: [Number] -> Number
promedio numeros = sum numeros / length numeros 

-- 2.MAGOS --
-- mago --> tiene hechizos y cant. de horrocruxes

-- A) Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un hechizo sobre un postre (se espera obtener el mago). 
--  (Cuando un mago practica con un hechizo --> lo agrega a sus hechizos aprendidos) 
-- Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada kedavra” al postre, entonces suma un horrorcrux.

data Mago = UnMago {
    nombrecito :: String,
    hechizos :: [Hechizo],
    cantDeHorrorcruxes :: Number
}deriving Show

ezequiel :: Mago
ezequiel = UnMago "Ezequiel" [] 0

-- MI CHOCLO COMPILA, SOLO QUE ES MUY CHOCLO XD (ADEMAS QUE SE REPITE LOGICA Y BLA BLA)
defensaContraLasCocinasOscuras :: Hechizo -> Postre -> Mago -> Mago
defensaContraLasCocinasOscuras newhechizo postre mago 
    | hacerHechizo newhechizo postre == (avadakedavra postre) = mago {hechizos = newhechizo : hechizos mago, cantDeHorrorcruxes = cantDeHorrorcruxes mago + 1}
    | otherwise = mago {hechizos = newhechizo : hechizos mago}

-- IR A LA CLASE ESA SERIA PRACTICAR HECHIZO
-- ESTE ES MAS LINDO (CON MAS FUNCIONES AUXILIARES) 

practicarHechizo :: Hechizo -> Postre -> Mago -> Mago
practicarHechizo hechizo postre  = (agregarUnHorroCruxSegun hechizo postre) . (aprenderHechizo hechizo)

aprenderHechizo :: Hechizo -> Mago -> Mago
aprenderHechizo newHechizo mago = mago {hechizos = newHechizo : hechizos mago}

agregarUnHorroCruxSegun :: Hechizo -> Postre -> Mago -> Mago
agregarUnHorroCruxSegun hechizo postre mago
    | esEquivalenteAAvadakedavra hechizo postre = sumarUnHorrorcrux mago
    | otherwise = mago

esEquivalenteAAvadakedavra :: Hechizo -> Postre -> Bool
esEquivalenteAAvadakedavra hechizo postre = hechizo postre == avadakedavra postre

sumarUnHorrorcrux :: Mago -> Mago  
sumarUnHorrorcrux mago = mago {cantDeHorrorcruxes = cantDeHorrorcruxes mago + 1}

-- B) Dado un postre y un mago obtener su mejor hechizo, 
-- que es aquel de sus hechizos que deja al postre con más cantidad de sabores luego de usarlo.

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (mejorEntreDos postre) (hechizos mago)  

mejorEntreDos :: Postre -> Hechizo -> Hechizo -> Hechizo
mejorEntreDos postre hechizo1 hechizo2 
    | length (sabores (hechizo1 postre)) > length (sabores (hechizo2 postre)) = hechizo1
    | otherwise = hechizo1

esMejor :: Postre -> Hechizo -> Hechizo -> Bool   
esMejor postre hechizo1 hechizo2 = (length . sabores . hechizo1 ) postre > (length . sabores . hechizo2 ) postre 

