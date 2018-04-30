module MicroEntrega1 where

type Acumulador = Int

type Instruccion = (Microprocesador -> Microprocesador)

data Microprocesador = Microprocesador{modelo :: String, programCounter :: Int, acumuladorA :: Acumulador,
  acumuladorB :: Acumulador, memoria :: [Int], mensajeError :: String}

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
  acumuladorB = 0}

errorDivisionPorCero :: Instruccion
errorDivisionPorCero microprocesador = microprocesador{mensajeError = "DIVISION BY ZERO"}

realizarDivision :: Instruccion
realizarDivision microprocesador = microprocesador{acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador),
  acumuladorB = 0}

divide :: Instruccion
divide microprocesador
  |acumuladorB microprocesador == 0 = errorDivisionPorCero microprocesador
  |otherwise = realizarDivision microprocesador

swap :: Instruccion
swap microprocesador = microprocesador{acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador}

lodv :: Int -> Microprocesador -> Microprocesador --Aplicada parcialmente se convertirá en una instrucción
lodv valorACargar microprocesador = microprocesador{acumuladorA = valorACargar}
