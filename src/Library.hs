module Library where
import PdePreludat

data Casillero = Propiedad { nombre :: String, precio :: Number, hipoteca :: Number, cobro :: Number } | 
                 Esquina { nombre :: String } | 
                 PagoImpuestos { nombre :: String, impuesto :: Number } 
                 deriving (Show, Eq)

cabildoYJuramento = 
  Propiedad { nombre = "Cabildo y Juramento", precio = 1000, hipoteca = 700, cobro = 500}
cabildoYCongreso = 
  Propiedad { nombre = "Cabildo y Congreso", precio = 1200, hipoteca = 800, cobro = 550}
obelisco = 
  Propiedad { nombre = "Obelisco", precio = 2000, hipoteca = 1000, cobro = 1000}

carcel = Esquina "carcel"
descanso = Esquina "descanso"

impuestoPatrullaAntiOsos = PagoImpuestos "Patrulla anti osos" 5

data Posicion = Posicion { 
    ubicacion :: Number, 
    casillero :: Casillero 
    } deriving (Show)
posicion2 = Posicion { ubicacion = 2, casillero = cabildoYJuramento }
posicion3 = Posicion { ubicacion = 3, casillero = impuestoPatrullaAntiOsos }
posicion4 = Posicion { ubicacion = 4, casillero = cabildoYCongreso }
posicion11 = Posicion { ubicacion = 11, casillero = descanso }
posicion21 = Posicion { ubicacion = 21, casillero = carcel }
posicion31 = Posicion { ubicacion = 31, casillero = carcel }

tablero = [posicion2, posicion3, posicion4, posicion11, posicion21, posicion31] -- [1..40]

--1
type Propiedad = Casillero
data Jugador = Jugador{
    posicionActual :: Number,
    propiedades:: [Propiedad],
    dinero :: Number
} deriving (Show)

leo = Jugador 4 [cabildoYCongreso] 1300
mati = Jugador 10 [] 2000

listaDeJugadores = [leo, mati]
--2
buscarPosicionPara numero = (head.filter ((==numero).ubicacion)) tablero
--3
tieneLaPropiedad jugador propiedad =elem propiedad $ propiedades jugador

--4
--a
cuantoCobraPor propiedad jugador
    | tieneLaPropiedad jugador propiedad = cobro propiedad
    | otherwise = 0

--b
hipotecar jugador propiedad
    | tieneLaPropiedad jugador propiedad = hipotecar quitarPropiedad
    | otherwise = jugador
    where 
        quitarPropiedad = jugador {propiedades = filter (/=propiedad) $ propiedades jugador}
        hipotecar jugadorRecibido= jugadorRecibido { dinero = dinero jugador + hipoteca propiedad}

--5 a
casilleroActual jugador = casillero.buscarPosicionPara $ posicionActual jugador

--b
estaParadoEn casillero= (==casillero).casilleroActual

--6 a
puedeComprar jugador propiedadAComprar@Propiedad {}= ningunJugadorLaTiene && tieneSuficienteDinero && estaSobreLaPropiedad
    where 
        ningunJugadorLaTiene = not.any (`tieneLaPropiedad` propiedadAComprar) $ listaDeJugadores
        tieneSuficienteDinero = precio propiedadAComprar <= dinero jugador
        estaSobreLaPropiedad = casilleroActual jugador == propiedadAComprar
puedeComprar jugador _ = False

--b
intentarComprar jugador propiedadAComprar@Propiedad {}
    | puedeComprar jugador propiedadAComprar = jugador{
        dinero = dinero jugador - precio propiedadAComprar,
        propiedades = propiedades jugador ++ [propiedadAComprar]}
    | otherwise = jugador

--7
intentarPagar jugador montoAPagar
    |dinero jugador >= montoAPagar = jugador {dinero=dinero jugador-montoAPagar}
    |otherwise = procesoDeHipotecar (propiedades jugador) montoAPagar jugador


procesoDeHipotecar [] montoAPagar jugador= eliminarJugador jugador
procesoDeHipotecar (propiedad : resto) montoAPagar jugador
    | dinero jugadorTrasHipoteca >= montoAPagar = jugadorTrasHipoteca { dinero = dinero jugadorTrasHipoteca - montoAPagar }
    | otherwise = procesoDeHipotecar resto montoAPagar jugadorTrasHipoteca
    where jugadorTrasHipoteca = hipotecar jugador propiedad

eliminarJugador jugador = jugador{
    posicionActual = 0,
    dinero=0,
    propiedades = []
}
    --devuelve al jugador con el monto pagado
--8
funcionSiniestra x y z = all x . map ((+5) . y . z)
{-
x es una funcion booleana, ya que es el segundo parametro de un all.
tanto y como z son funciones de tipo numerico ya que se componene con una funcion (+5) dentro del map.
La funcion recibe una lista de numeros, se le aplica la composicion (+5) . y . z y luego a esa lista se
"introduce" en el all x para evaluarla. Si todos sus elementos la cumplen, entonces devuelve true, caso contrario false.
-}
--CAMBIO DE PRUEBA