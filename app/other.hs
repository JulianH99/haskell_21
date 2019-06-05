import System.Random

escribir::String->IO ()
escribir m= do putStr m
               putStr "\n"

getInt :: IO Int
getInt = do line <- getLine
            return (read line :: Int)

aleatorio::Int -> Int ->IO Int
aleatorio a b = randomRIO(a,b) :: IO Int

tomaCartaMaso :: [(Int, String)] -> Int -> (Int, String)
tomaCartaMaso m a = m !! a

quitarCarta :: [(Int, String)] -> (Int,String) -> [(Int, String)]
quitarCarta m a = 
    [x | x <- m, a/=x]

agregarCarta :: [(Int, String)] -> (Int,String) -> [(Int, String)]
agregarCarta m carta = m ++ [carta]

sumaMano :: [(Int, String)] -> Int
sumaMano [ ] = 0
sumaMano (x:xs) = fst x + sumaMano xs

sumaReal :: Int -> [(Int, String)] -> Int
sumaReal suma cartas    
    | suma + 10 < 21 && not(null cartas) = sumaReal (suma+10) cartas1
    | otherwise = suma
    where cartas1 = quitarCarta cartas (head cartas)
    
filtroUnos :: [(Int, String)] -> [(Int, String)]
filtroUnos m = [x | x <- m, fst x == 1]    
        

juego :: [(Int, String)] -> [(Int, String)] -> [(Int, String)] -> IO()
juego maso manoJ manoPC = do
    x <- aleatorio 0 (length maso -1)
    let cartaJ = tomaCartaMaso maso x
    y <- aleatorio 0 (length (quitarCarta maso cartaJ)-1)
    let cartaPC = tomaCartaMaso (quitarCarta maso cartaJ) y
    escribir "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
    escribir "Mano Jugador:"    
    print(agregarCarta manoJ cartaJ)
    escribir "Mano PC tapada:"
    print(quitarCarta (agregarCarta manoPC cartaPC) (head(agregarCarta manoPC cartaPC)))
    escribir "desea pedir carta?: (1: si, 2: no)"
    n <- getInt
    if n == 1 then 
        juego (quitarCarta (quitarCarta maso cartaJ) cartaPC) (agregarCarta manoJ cartaJ) (agregarCarta manoPC cartaPC)
    else (do 
        let puntajeJugador = sumaReal (sumaMano(agregarCarta manoJ cartaJ)) (filtroUnos manoJ)
        let puntajePC = sumaReal (sumaMano(agregarCarta manoPC cartaPC)) (filtroUnos manoPC)
        escribir "Puntaje jugador: "    
        print puntajeJugador
        escribir "Puntaje PC: "    
        print puntajePC
        if puntajeJugador > 21 then
            escribir "te pasaste :(.. pierdes"
        else if puntajeJugador == 21 then
            escribir "ganaste :)"
        else if puntajePC == 21 then
            escribir "gana el pc .. :("
        else
            if puntajePC<21 then
                if puntajeJugador > puntajePC then
                    escribir "Tu ganas :)"
                else if puntajeJugador < puntajePC then
                    escribir "te pasaste :(.. pierdes"
                else
                escribir "empatados"      
            else
                escribir "Tu ganas :)"

        escribir "Mano PC destapada:"
        print(agregarCarta manoPC cartaPC)
                          
        escribir "fin del juego")  

maso = [(1,"corazones"),(2,"corazones"),(3,"corazones"),(4,"corazones"),(5,"corazones"),(6,"corazones"),(7,"corazones"),(8,"corazones"),(9,"corazones"),(10,"corazones"),(10,"corazones"),(10,"corazones"),(10,"corazones"),(1,"diamantes"),(2,"diamantes"),(3,"diamantes"),(4,"diamantes"),(5,"diamantes"),(6,"diamantes"),(7,"diamantes"),(8,"diamantes"),(9,"diamantes"),(10,"diamantes"),(10,"diamantes"),(10,"diamantes"),(10,"diamantes"),(1,"picas"),(2,"picas"),(3,"picas"),(4,"picas"),(5,"picas"),(6,"picas"),(7,"picas"),(8,"picas"),(9,"picas"),(10,"picas"),(10,"picas"),(10,"picas"),(10,"picas"),(1,"treboles"),(2,"treboles"),(3,"treboles"),(4,"treboles"),(5,"treboles"),(6,"treboles"),(7,"treboles"),(8,"treboles"),(9,"treboles"),(10,"treboles"),(10,"treboles"),(10,"treboles"),(10,"treboles")]

manoJugador = []
manoPC = []



main :: IO ()
main = juego maso manoJugador manoPC