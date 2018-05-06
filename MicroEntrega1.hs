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

{- EJERCICIOS EN CONSOLA
________________________________________________________________________
3.2.2
Desde la consola, modele un programa que haga avanzar tres 
posiciones el program counter.

*MicroEntrega1> (nop.nop.nop) xt8088
Modelo: xt8088
Program counter: 3
Acumulador A: 0
Acumulador B: 0
Memoria: []
Mensaje de error : 

¿Qué concepto interviene para lograr este punto?
Composición
________________________________________________________________________
3.3.2
Implementar el siguiente programa, que permite sumar 10 + 22

LODV 10   // Cargo el valor 10 en el acumulador A
SWAP      // Cargo el valor 10 en el acumulador B (paso de A a B)
LODV 22   // Cargo el valor 22 en el acumulador A
ADD       // Realizo la suma y el resultado queda en el acumulador A

*MicroEntrega1> ( add . lodv 22 . swap . lodv 10 ) xt8088
Modelo: xt8088
Program counter: 4
Acumulador A: 32
Acumulador B: 0
Memoria: []
Mensaje de error : 
_______________________________________________________________________
3.4.2
Desde la consola, modele un programa que intente dividir 2 por 0.

STR  1 2  // Guardo en la posición 1 de memoria el valor 2
STR  2 0  // Guardo en la posición 2 de memoria el valor 0
LOD  2    // Cargo en el acumulador A el valor 0 (pos.2)
SWAP      // Guardo el valor 0 en el acumulador B
LOD  1    // Cargo en el acumulador A el valor 2 (pos.1)
DIV       // Intento hacer la división

El microprocesador debe tener en la etiqueta de error el 
mensaje “DIVISION BY ZERO” y el Program Counter debe quedar 
en 6 (el índice de la instrucción donde ocurrió el error).

*MicroEntrega1> ( divide . lod 1 . swap . lod 2 . str 2 0 . str 1 2 ) xt8088
Modelo: xt8088
Program counter: 6
Acumulador A: 2
Acumulador B: 0
Memoria: [2,0]
Mensaje de error : DIVISION BY ZERO
______________________________________________________________________
CASOS DE PRUEBA
______________________________________________________________________

4.2 Punto 3
LODV 5 tiene 
como precondiciones: el acumulador A y B están en cero
como post-condiciones: el acumulador A tiene valor 5 y el B cero.

*MicroEntrega1> lodv 5 xt8088
Modelo: xt8088
Program counter: 1
Acumulador A: 5
Acumulador B: 0
Memoria: []
Mensaje de error :
______________________________________________________________________

4.2 Punto 3
Dado un procesador fp20 que tiene acumulador A con 7 y
acumulador B con 24, al ejecutar SWAP el acumulador A 
debe quedar con 24 y el B con 7.

*MicroEntrega1> swap fp20
Modelo: fp20
Program counter: 1
Acumulador A: 24
Acumulador B: 7
Memoria: []
Mensaje de error :
_____________________________________________________________________

4.3 Punto 4
Dado el procesador at8086 que tiene los acumuladores en cero, 
el program counter en 0, sin mensaje de error y una memoria con 
los siguientes datos: [1..20], le ejecutamos la instrucción 
STR 2 5. Entonces el procesador at8086 debe quedar con un 5 en la 
posición 2: [1, 5, 3, 4, 5,... ]

*MicroEntrega1> str 2 5 at8086
Modelo: at8086
Program counter: 1
Acumulador A: 0
Acumulador B: 0
Memoria: [1,5,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
Mensaje de error : 
_____________________________________________________________________

4.3 Punto 4
Ejecutar la división de 12 por 4 para el procesador xt8088 
que debe dar 3 y no tirar ningún mensaje de error.

*MicroEntrega1> ( divide . lod 1 . swap . lod 2 . str 2 4 . str 1 12 ) xt8088
Modelo: xt8088
Program counter: 6
Acumulador A: 3
Acumulador B: 0
Memoria: [12,4]
Mensaje de error : 
____________________________________________________________________
-}
