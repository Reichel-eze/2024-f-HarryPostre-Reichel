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

-- 3.INFINITA MAGIA --

-- A) Construir una lista infinita de postres, y construir un mago con infinitos hechizos.

listaInfinitaDePostres :: [Postre]
listaInfinitaDePostres = cycle [bizcochoBorracho,tartaDeMelaza]

listaInfinitaDeBizcochos :: [Postre]
listaInfinitaDeBizcochos = repeat bizcochoBorracho

magoConHechizosInfinitos :: Mago
magoConHechizosInfinitos = UnMago "Mago Infinito" (cycle [incendio,immobulus]) 1


-- B) Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos 
-- ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar conceptualmente.

--dejaranListos :: [Postre] -> Hechizo -> Bool
--dejaranListos postres hechizo = all (estaListo . hechizo) postres

-- Como la funcion dejaranListos evalua con una lista de postres y un hechizo, si dicho hechizo los dejara listo a TODOS. 
-- Como es necesario evaluar a TODOS los postres de la lista para llegar a un resultado (True = si los deja listos a TODOS, False = en el caso contrario)
-- CASO DEL TRUE --> Entonces, si la lista de los postres es INFINITA, nunca voy a poder llegar a verificar para cada uno de los postres si cumplen la condicion de estarListo
-- CASO DEL FALSE --> PEROO, aunque la lista sea infinita, con que uno de los postres ya NO cumpla con la condicion de estarListo, entonces no va a ser necesario seguir recorriendo la lista
-- (si un postre NO estaListo ---> entonces TODOS los prostes NO estanListos (ya que tengo ese postre que NO ESTA LISTO dentro de el conjunto))
-- Esto es posible gracias a la evaluacion perozosa que tiene Haskell, como encuentre uno que tire False en estaListo entonces corta y devuelve False en dejarListos (NO sigue recorrindo la lista infinita)  

-- Ejemplo de consulta:
-- > dejaranListos listaInfinitaDeBizcochos immobulus
-- FALSE

-- Ejemplo de consulta:
-- > dejaranListos listaInfinitaDeBizcochos avadakedavra
-- FALSE

-- C) Suponiendo que un mago tiene infinitos hechizos 
-- ¿Existe algún caso en el que se puede encontrar al mejor hechizo? Justificar conceptualmente.

--mejorHechizo :: Postre -> Mago -> Hechizo
--mejorHechizo postre mago = foldl1 (mejorEntreDos postre) (hechizos mago)  

--mejorEntreDos :: Postre -> Hechizo -> Hechizo -> Hechizo
--mejorEntreDos postre hechizo1 hechizo2 
--    | length (sabores (hechizo1 postre)) > length (sabores (hechizo2 postre)) = hechizo1
--    | otherwise = hechizo1

-- NO hay ningun caso que pueda encontrar el mejor hechizo de un mago con hechizos infinitos, 
-- porque para buscar el mejor tengo que recorrer toda la lista de hechizos que tiene el mago para ir comparando entre ellos, 
-- Es decir, aunque Haskell tenga un evaluacion perezosa (lazy evaluation), en esta caso NO tengo escapacion, tengo que evaluar
-- todos los elementos de la lista :( !!
