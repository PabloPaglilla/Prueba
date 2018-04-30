module MicroEntrega1 where

type Acumulador = Int
type Memoria = [Int]
type Instruccion = (Microprocesador -> Microprocesador)

data Microprocesador = Microprocesador{modelo :: String, programCounter :: Int, acumuladorA :: Acumulador,
  acumuladorB :: Acumulador, memoria :: Memoria, mensajeError :: String}

instance Show Microprocesador where
  show microprocesador = "Modelo: " ++ modelo microprocesador ++ "\nProgram counter: " ++ (show.programCounter) microprocesador ++
    "\nAcumulador A: " ++ (show.acumuladorA) microprocesador ++ "\nAcumulador B: " ++ (show.acumuladorB) microprocesador ++
    "\nMemoria: " ++ (show.memoria) microprocesador ++ "\nMensaje de error : " ++ mensajeError microprocesador

xt8088 = Microprocesador{modelo = "xt8088", programCounter = 0, acumuladorA = 0, acumuladorB = 0,
  memoria = [], mensajeError = ""}
  
at8086 = Microprocesador{modelo = "at8086", programCounter = 0, acumuladorA = 0, acumuladorB = 0,
  memoria = [1 .. 20], mensajeError = ""}

fp20 = Microprocesador{modelo = "fp20", programCounter = 0, acumuladorA = 7, acumuladorB = 24,
  memoria = [], mensajeError = ""}

nop :: Instruccion
nop microprocesador = microprocesador{programCounter = programCounter microprocesador + 1}

add :: Instruccion
add microprocesador = microprocesador{acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador,
  acumuladorB = 0, programCounter = programCounter microprocesador + 1}

errorDivisionPorCero :: Instruccion
errorDivisionPorCero microprocesador = microprocesador{mensajeError = "DIVISION BY ZERO",
  programCounter = programCounter microprocesador + 1}

realizarDivision :: Instruccion
realizarDivision microprocesador = microprocesador{acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador),
  acumuladorB = 0, programCounter = programCounter microprocesador + 1}

divide :: Instruccion
divide microprocesador
  |acumuladorB microprocesador == 0 = errorDivisionPorCero microprocesador
  |otherwise = realizarDivision microprocesador

swap :: Instruccion
swap microprocesador = microprocesador{acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador,
  programCounter = programCounter microprocesador + 1}

lodv :: Int -> Microprocesador -> Microprocesador
lodv valorACargar microprocesador = microprocesador{acumuladorA = valorACargar, programCounter = programCounter microprocesador + 1}

cargarMemoriaEnAcumuladorA :: Int -> Microprocesador -> Microprocesador
cargarMemoriaEnAcumuladorA direccion microprocesador = microprocesador{acumuladorA = (memoria microprocesador) !! (direccion - 1),
  programCounter = programCounter microprocesador + 1}

errorMemoriaNoInicializada :: Instruccion
errorMemoriaNoInicializada microprocesador = microprocesador{mensajeError = "Posicion de memoria especificada no inicializada",
  programCounter = programCounter microprocesador + 1}

lod :: Int -> Microprocesador -> Microprocesador
lod direccion microprocesador
  |(length.memoria) microprocesador >= direccion = cargarMemoriaEnAcumuladorA direccion microprocesador
  |otherwise = errorMemoriaNoInicializada microprocesador

cargarValorEnMemoria :: Int -> Int -> Memoria -> Memoria
cargarValorEnMemoria direccion valor memoria = (take (direccion - 1) memoria) ++ (valor : drop direccion memoria)

extenderMemoria :: Int -> Memoria -> Memoria
extenderMemoria direccion memoria = memoria ++ (replicate (direccion - length memoria) 0)

inicializarMemoria :: Int -> Int -> Microprocesador -> Microprocesador
inicializarMemoria direccion valor microprocesador = microprocesador{
  memoria = cargarValorEnMemoria direccion valor (extenderMemoria direccion (memoria microprocesador)),
  programCounter = programCounter microprocesador + 1}

str :: Int -> Int -> Microprocesador -> Microprocesador
str direccion valor microprocesador
  |(length.memoria) microprocesador >= direccion = microprocesador{memoria = cargarValorEnMemoria direccion valor
    (memoria microprocesador), programCounter = programCounter microprocesador + 1}
  |otherwise = inicializarMemoria direccion valor microprocesador
