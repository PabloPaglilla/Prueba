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

depurarPrograma microprocesador = microprocesador{programa =
  depuracionRecursiva (programa microprocesador) microprocesador []}

--Depuracion recursiva recibe una lista vacia como tercer parametro, en la cual cargará
--recursivamente las instrucciones que conformen el programa depurado para luego devolverla
depuracionRecursiva :: Programa -> Microprocesador -> Programa -> Programa
depuracionRecursiva [] _ programaDepurado = programaDepurado
depuracionRecursiva programa microprocesador programaDepurado
  |acumuladoresYMemoriaEn0 ((siguienteInstruccion programa) microprocesador) =
    depuracionRecursiva (instruccionesRestantes programa) ((siguienteInstruccion programa) microprocesador) programaDepurado
  |otherwise = depuracionRecursiva (instruccionesRestantes programa) ((siguienteInstruccion programa) microprocesador) 
    (programaDepurado ++ [(siguienteInstruccion programa)])

primerosDosElementosOrdenados :: Memoria -> Bool
primerosDosElementosOrdenados (x:xs) = x <= head xs

memoriaOrdenada :: Microprocesador -> Bool
memoriaOrdenada microprocesador
  |length (memoria microprocesador) <= 1 = True
  |primerosDosElementosOrdenados (memoria microprocesador) = memoriaOrdenada microprocesador{memoria = tail (memoria microprocesador)}
  |otherwise = False
