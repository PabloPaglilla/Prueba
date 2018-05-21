module MicroEntrega1 where

import Text.Show.Functions

type Acumulador = Int
type Memoria = [Int]
type Instruccion = (Microprocesador -> Microprocesador)
type Programa = [Instruccion]

data Microprocesador = Microprocesador{modelo :: String, programCounter :: Int, acumuladorA :: Acumulador,
  acumuladorB :: Acumulador, memoria :: Memoria, programa :: Programa, mensajeError :: String}

instance Show Microprocesador where
  show microprocesador = "Modelo: " ++ modelo microprocesador ++ "\nProgram counter: " ++ (show.programCounter) microprocesador ++
    "\nAcumulador A: " ++ (show.acumuladorA) microprocesador ++ "\nAcumulador B: " ++ (show.acumuladorB) microprocesador ++
    "\nMemoria: " ++ (show.memoria) microprocesador ++
    "\nCantidad de instrucciones del programa: " ++ (show.length.programa) microprocesador ++ 
    "\nMensaje de error : " ++ mensajeError microprocesador

xt8088 :: Microprocesador
xt8088 = Microprocesador{modelo = "xt8088", programCounter = 0, acumuladorA = 0, acumuladorB = 0,
  memoria = [], mensajeError = "", programa = []}

at8086 :: Microprocesador  
at8086 = Microprocesador{modelo = "at8086", programCounter = 0, acumuladorA = 0, acumuladorB = 0,
  memoria = [1 .. 20], mensajeError = "", programa = []}

fp20 :: Microprocesador
fp20 = Microprocesador{modelo = "fp20", programCounter = 0, acumuladorA = 7, acumuladorB = 24,
  memoria = [], mensajeError = "", programa = []}

microMemoriaInfinita :: Microprocesador
microMemoriaInfinita = Microprocesador{modelo = "memoriaInfinita", programCounter = 0, acumuladorA = 0,
  acumuladorB = 0, memoria = [0, 0 ..], mensajeError = "", programa = []}

sumar10Y22 :: Programa
sumar10Y22 = [lodv 10, swap, lodv 22, add]

dividir2Por0 :: Programa
dividir2Por0 = [str 1 2, str 2 0, lod 2, swap, lod 1, divide]

aumentarProgramCounter microprocesador = microprocesador {programCounter = programCounter microprocesador + 1}

nop :: Instruccion
nop = aumentarProgramCounter

add :: Instruccion
add microprocesador = aumentarProgramCounter microprocesador{
  acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador, acumuladorB = 0}

errorDivisionPorCero :: Instruccion
errorDivisionPorCero microprocesador = aumentarProgramCounter microprocesador{mensajeError = "DIVISION BY ZERO"}

realizarDivision :: Instruccion
realizarDivision microprocesador = aumentarProgramCounter microprocesador{acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador),
  acumuladorB = 0}

divide :: Instruccion
divide microprocesador
  |(acumuladorB microprocesador == 0) = errorDivisionPorCero microprocesador
  |otherwise = realizarDivision microprocesador

swap :: Instruccion
swap microprocesador = aumentarProgramCounter microprocesador{acumuladorA = acumuladorB microprocesador,
  acumuladorB = acumuladorA microprocesador}

lodv :: Int -> Instruccion
lodv valorACargar microprocesador = aumentarProgramCounter microprocesador{acumuladorA = valorACargar}

cargarMemoriaEnAcumuladorA :: Int -> Instruccion
cargarMemoriaEnAcumuladorA direccion microprocesador = aumentarProgramCounter microprocesador{
  acumuladorA = (memoria microprocesador) !! (direccion - 1)}

errorMemoriaNoInicializada :: Instruccion
errorMemoriaNoInicializada microprocesador = aumentarProgramCounter microprocesador{
  mensajeError = "Posicion de memoria especificada no inicializada"}

lod :: Int -> Instruccion
lod direccion microprocesador
  |((length.memoria) microprocesador >= direccion) = cargarMemoriaEnAcumuladorA direccion microprocesador
  |otherwise = errorMemoriaNoInicializada microprocesador

cargarValorEnMemoria :: Int -> Int -> Memoria -> Memoria
cargarValorEnMemoria direccion valor memoria = (take (direccion - 1) memoria) ++ (valor : drop direccion memoria)

extenderMemoria :: Int -> Memoria -> Memoria
extenderMemoria direccion memoria = memoria ++ (replicate (direccion - length memoria) 0)

inicializarMemoria :: Int -> Int -> Instruccion
inicializarMemoria direccion valor microprocesador = aumentarProgramCounter microprocesador{
  memoria = cargarValorEnMemoria direccion valor (extenderMemoria direccion (memoria microprocesador))}

str :: Int -> Int -> Instruccion
str direccion valor microprocesador
  |((length.memoria) microprocesador >= direccion) = aumentarProgramCounter microprocesador{
    memoria = cargarValorEnMemoria direccion valor (memoria microprocesador)}
  |otherwise = inicializarMemoria direccion valor microprocesador

cargarPrograma :: Programa -> Microprocesador -> Microprocesador
cargarPrograma programa microprocesador = microprocesador{programa = programa}

siguienteInstruccion :: Programa -> Instruccion
siguienteInstruccion = head

instruccionesRestantes :: Programa -> Programa
instruccionesRestantes = tail

ejecutarPrograma :: Instruccion
ejecutarPrograma microprocesador = ejecucionRecursiva (programa microprocesador) microprocesador

ejecucionRecursiva :: Programa -> Microprocesador -> Microprocesador
ejecucionRecursiva [] microprocesador = microprocesador
ejecucionRecursiva programa microprocesador
  |(mensajeError microprocesador /= "") = microprocesador
  |otherwise = ejecucionRecursiva (instruccionesRestantes programa) ((siguienteInstruccion programa) microprocesador)

ifnz :: Instruccion
ifnz microprocesador
  |acumuladorA microprocesador == 0 = microprocesador
  |otherwise = ejecutarPrograma microprocesador

memoriaEn0 :: Microprocesador -> Bool
memoriaEn0 = (all (== 0)).memoria

acumuladoresYMemoriaEn0 :: Microprocesador -> Bool
acumuladoresYMemoriaEn0 microprocesador = (acumuladorA microprocesador == 0) && (acumuladorB microprocesador == 0) &&
  (memoriaEn0 microprocesador)

depurarPrograma :: Instruccion
depurarPrograma microprocesador = microprocesador{programa =
  depuracionRecursiva (programa microprocesador) microprocesador []}

--Depuracion recursiva recibe una lista vacia como tercer parametro, en la cual cargará
--recursivamente las instrucciones que conformen el programa depurado para luego devolverla
depuracionRecursiva :: Programa -> Microprocesador -> Programa -> Programa
depuracionRecursiva [] _ programaDepurado = programaDepurado
depuracionRecursiva programa microprocesador programaDepurado
  |acumuladoresYMemoriaEn0 ((siguienteInstruccion programa) microprocesador) =
    depuracionRecursiva (instruccionesRestantes programa) microprocesador programaDepurado
  |otherwise = depuracionRecursiva (instruccionesRestantes programa) microprocesador 
    (programaDepurado ++ [(siguienteInstruccion programa)])

primerosDosElementosOrdenados :: Memoria -> Bool
primerosDosElementosOrdenados (x:xs) = x <= head xs

memoriaOrdenada :: Microprocesador -> Bool
memoriaOrdenada microprocesador
  |length (memoria microprocesador) <= 1 = True
  |primerosDosElementosOrdenados (memoria microprocesador) = memoriaOrdenada microprocesador{memoria = tail (memoria microprocesador)}
  |otherwise = False
  
{- EJERCICIOS EN CONSOLA
______________________________________________________________________
CASOS DE PRUEBA
______________________________________________________________________
4.2 Punto 1
(ejecutarPrograma.(cargarPrograma sumar10Y22)) xt8088 
como precondiciones: el acumulador A y B están en cero
como post-condiciones: el acumulador A tiene valor 32, el B cero y programCounter en 4
*MicroEntrega1> (ejecutarPrograma.(cargarPrograma sumar10Y22)) xt8088
Modelo: xt8088
Program counter: 4
Acumulador A: 32
Acumulador B: 0
Memoria: []
Cantidad de instrucciones del programa: 4
Mensaje de error :
______________________________________________________________________
4.2 Punto 2
(ejecutarPrograma.(cargarPrograma dividir2Por0)) xt8088
como precondiciones: el acumulador A y B están en cero
como post-condiciones: el acumulador A tiene valor 2, el B cero y programCounter en 6. Debe haber mensaje de error dada la division por cero. Los primeros dos elementos de la memoria de datos deben ser 2 y 0
*MicroEntrega1> (ejecutarPrograma.(cargarPrograma dividir2Por0)) xt8088
Modelo: xt8088
Program counter: 6
Acumulador A: 2
Acumulador B: 0
Memoria: [2,0]
Cantidad de instrucciones del programa: 6
Mensaje de error : DIVISION BY ZERO
______________________________________________________________________
4.3 Punto 1
(ifnz.(cargarPrograma [(lodv 3),swap])) fp20
como precondiciones: el acumulador A esta en 7 y el B en 24
como post-condiciones: el acumulador A tiene valor 24 y el B tiene 3
*MicroEntrega1> (ifnz.(cargarPrograma [(lodv 3),swap])) fp20
Modelo: fp20
Program counter: 2
Acumulador A: 24
Acumulador B: 3
Memoria: []
Cantidad de instrucciones del programa: 2
Mensaje de error : 
______________________________________________________________________
4.3 Punto 2
(ifnz.(cargarPrograma [(lodv 3),swap])) xt8088
como precondiciones: el acumulador A esta en 0 y el B en 
como post-condiciones: el acumulador A y B deben quedar en cero
*MicroEntrega1> (ifnz.(cargarPrograma [(lodv 3),swap])) xt8088
Modelo: xt8088
Program counter: 0
Acumulador A: 0
Acumulador B: 0
Memoria: []
Cantidad de instrucciones del programa: 2
Mensaje de error : 
______________________________________________________________________
4.4
Se depura un programa:
*MicroEntrega1> (depurarPrograma.(cargarPrograma [swap,nop,(lodv 133),(lodv 0),(str 1 3),(str 2 0)])) xt8088
Modelo: xt8088
Program counter: 0
Acumulador A: 0
Acumulador B: 0
Memoria: []
Cantidad de instrucciones del programa: 2
Mensaje de error : 
Y me quedan dos instrucciones 
______________________________________________________________________
4.5
Orden de la memoria: 
*MicroEntrega1> memoriaOrdenada xt8088
True
*MicroEntrega1> memoriaOrdenada microDesorden
False
____________________________________________________________________
-}
