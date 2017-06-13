module Main where

data Input = Plaster [String] deriving (Read, Show)    


--wczytanie pliku
main = do
  print "Podaj nazwe pliku z rozszerzeniem"
  path <- getLine
  Plaster xs <- readIO =<< readFile path
  _ <- traverse print xs --(zip [0 :: Int ..] xs)
  checkIsOdd xs

--sprawdzenie ilosci linii w pliku - musi byc nieparzysta
checkIsOdd xs = do 
    let lng = length xs
    --print lng 
    if odd lng == False then
        print "Bledny plik - liczba wierszy musi byc nieparzysta"
    else
        checkLine xs lng 0

--sprawdzenie ilosci znakow w linii
checkLine xs lng count = if count > lng-1 then do 
                                let avbLets = ['A', 'B', 'C', 'D', 'E', 'F', 'G']
                                let changes = []
                                trySolve xs lng 0 avbLets changes
                             else
                                if even count == True then
                                    if length (xs !! count) == lng-1 then
                                        checkLineEven xs lng count 0--checkLine xs lng (count+1)
                                    else 
                                        print $ "Bledna " ++ show (count+1) ++ " linia pliku - niewlasciwa ilosc znakow" 
                                else
                                    if length (xs !! count) == lng then
                                        checkLineOdd xs lng count 0
                                    else
                                        print $ "Bledna " ++ show (count+1) ++ " linia pliku - niewlasciwa ilosc znakow"

checkLineOdd xs lng count loop =
    if loop < length (xs !! count) then
        if loop == 0 then
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop)) then
                    checkLineOdd xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
            else if loop == (length (xs !! count))-1 then
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop-1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop-1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop-1)) then
                    checkLineOdd xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
            else
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop-1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop-1)) &&
                    ((xs !! count) !! loop) /= ((xs !! count) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop-1))  then
                    checkLineOdd xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
    else
        checkLine xs lng (count+1)

--sprawdzenie poprawnosci pliku - czy znaki na wejsciu nie sasiaduja ze soba (linia parzysta)
checkLineEven xs lng count loop = do 
    if loop < length (xs !! count) then
        if count == 0 then
            if loop == 0 then
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop+1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
            else if loop == (length (xs !! count))-1 then
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop-1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop+1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
            else
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! count) !! (loop-1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
        else if count == (lng-1) then
            if loop == 0 then
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop+1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
            else if loop == (length (xs !! count))-1 then
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop-1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop-1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
            else
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! count) !! (loop-1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
        else
            if loop == 0 then
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count)) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop+1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
            else if loop == (length (xs !! count))-1 then
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count)) !! (loop-1)) &&
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop+1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
            else
                if ((xs !! count) !! loop) == '.' || ((xs !! count) !! loop) /= ((xs !! count) !! (loop-1)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! loop) && 
                    ((xs !! count) !! loop) /= ((xs !! (count-1)) !! (loop+1)) && 
                    ((xs !! count) !! loop) /= ((xs !! count) !! (loop+1)) &&  
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop)) && 
                    ((xs !! count) !! loop) /= ((xs !! (count+1)) !! (loop+1)) then
                    checkLineEven xs lng count (loop+1)
                else
                    print $ "Bledny plik - znak " ++ show(loop+1) ++ " w " ++ show(count+1) ++ " linii sasiaduje z takim samym znakiem"
    else
        checkLine xs lng (count+1)

--wyszukanie kropek w liniach - jesli sa, wywolanie wstawiania liter, jesli nie, przejscie do nastepnej linii. Po zakonczeniu wypisanie wyniku
trySolve xs lng count avbLets changes =  if count < lng then do 
                            let test = elem '.' (xs !! count)
                            if test then do 
                               print $ "Dots in line " ++ show(count+1)
                               putLetters xs count lng avbLets changes
                            else do
                               print $ "No dots in line " ++ show(count+1)
                               trySolve xs lng (count+1) avbLets changes
                         else do 
                            print $ "Changes: " ++ show(changes)
                            print "Result: "
                            showResult xs 0

--wyswietlenie rezultatu
showResult xs count = 
    if count < (length xs) then do 
        print (xs !! count)
        showResult xs (count+1)
    else do
        print "Done."
         

--wstawienie litery z wybranego miejsca na pozycje kropki
putLetter list count lng pos avbLets loop changes = do 
    let letter = avbLets !! loop
    let newString = replaceNth pos letter (list !! count)
    let newList = replaceNth count newString list
    let newChanges = changes ++ [(count, pos, loop)]
    trySolve newList lng count avbLets newChanges

--znalezienie pozycji kropek, wywolanie sprawdzenia, jakie litery mozna w ich miejsce wstawic
putLetters list count lng avbLets changes = do 
    let position = findPos (list !! count) '.'
    print $ "Dots indexes: " ++ show (position)
    checkLetters list count lng (position !! 0) avbLets changes

--sprawdzenie czy linia jest parzysta czy nieparzysta
checkLetters list count lng pos avbLets changes =
    if odd count then do 
        checkOddLine list count lng pos avbLets 0 changes
    else 
        checkEvenLine list count lng pos avbLets 0 changes

--sprawdz sasiadow linii nieparzystej
checkNeighboursOdd list count pos   | pos == 0 = [((list !! (count-1)) !! pos), ((list !! count) !! (pos+1)), ((list !! (count+1)) !! pos)]
                                    | pos == (length (list !! count))-1 = [((list !! (count+1)) !! (pos-1)), ((list !! count) !! (pos-1)), ((list !! (count+1)) !! (pos-1))]
                                    | otherwise = [((list !! count) !! (pos-1)), ((list !! (count-1)) !! (pos-1)), ((list !! (count-1)) !! pos), 
                                                   ((list !! count) !! (pos+1)), ((list !! (count+1)) !! pos), ((list !! (count+1)) !! (pos-1))]

--dla linii nieparzystych, zaleznie od pozycji kropki i numeru wiersza sprawdzenie wszystkich mozliwych sasiadow        
checkOddLine list count lng pos avbLets loop changes = if loop < 7 then
    if pos == 0 then do 
        let neighbours = checkNeighboursOdd list count pos ++ checkNeighboursEven list (count-1) pos ++ 
                         checkNeighboursOdd list count (pos+1) ++ checkNeighboursEven list (count+1) pos
        if (avbLets !! loop) `elem` neighbours then
            checkOddLine list count lng pos avbLets (loop+1) changes    
        else
            putLetter list count lng pos avbLets loop changes
    else if pos == ((length (list !! count))-1) then do
        let neighbours = checkNeighboursOdd list count pos ++ checkNeighboursEven list (count-1) (pos-1) ++
                         checkNeighboursOdd list count (pos-1) ++ checkNeighboursEven list (count+1) (pos-1)
        if (avbLets !! loop) `elem` neighbours then
            checkOddLine list count lng pos avbLets (loop+1) changes    
        else
            putLetter list count lng pos avbLets loop changes
    else do 
        let neighbours = checkNeighboursOdd list count pos ++ checkNeighboursOdd list count (pos-1) ++ checkNeighboursOdd list count (pos+1) ++
                         checkNeighboursEven list (count-1) pos ++ checkNeighboursEven list (count-1) (pos-1) ++
                         checkNeighboursEven list (count+1) pos ++ checkNeighboursEven list (count+1) (pos-1) 
        if (avbLets !! loop) `elem` neighbours then
            checkOddLine list count lng pos avbLets (loop+1) changes    
        else
            putLetter list count lng pos avbLets loop changes
  else do 
      print "No letters available"
      goBack list count lng pos avbLets loop changes 

goBack list count lng pos avbLets loop changes = do 
    let change = last changes
    let line = extractFirst change 
    let num = extractSecond change
    let letter = '.'
    let newString = replaceNth num letter (list !! line)
    let newList = replaceNth line newString list 
    print changes
    print $ "Went back to (" ++ show(line) ++ ", " ++ show(num) ++ ")"
    let loopCount = extractThird change 
    let newChanges = removeLast changes
    if even line then
        checkEvenLine newList line lng num avbLets (loopCount+1) newChanges
    else
        checkOddLine newList line lng num avbLets (loopCount+1) newChanges

extractFirst (a,_,_) = a
extractSecond (_,a,_) = a
extractThird (_,_,a) = a

removeLast [] = []
removeLast [(_,_,_)] = []
removeLast ((x,y,z):xs) = [(x,y,z)] ++ removeLast xs

--sprawdz sasiadow linii parzystej
checkNeighboursEven list count pos | count == 0 && pos == 0 = [((list !! (count)) !! (pos+1)), ((list !! (count+1)) !! pos), ((list !! (count+1)) !! (pos+1))]
                                   | count == 0 && pos == (length (list !! count))-1 = [((list !! (count)) !! (pos-1)), ((list !! (count+1)) !! pos), ((list !! (count+1)) !! (pos+1))]
                                   | count == 0 && pos > 0 && pos < (length (list !! count))-1 = [((list !! (count)) !! (pos-1)), ((list !! (count+1)) !! pos), ((list !! (count+1)) !! (pos+1)), ((list !! (count)) !! (pos+1))]
                                   | count == (length list)-1 && pos == 0 = [((list !! (count-1)) !! pos), ((list !! (count-1)) !! (pos+1)), ((list !! count) !! (pos+1))]
                                   | count == (length list)-1 && pos == (length (list !! count))-1 = [((list !! (count)) !! (pos-1)), ((list !! (count-1)) !! pos), ((list !! (count-1)) !! (pos+1))]
                                   | count == (length list)-1 && pos < (length (list !! count))-1 && pos > 0 = [((list !! (count)) !! (pos-1)), ((list !! (count-1)) !! pos), ((list !! (count-1)) !! (pos+1)), ((list !! (count)) !! (pos+1))]
                                   | count > 0 && count < (length list)-1 && pos == 0 = [((list !! (count-1)) !! pos), ((list !! (count-1)) !! (pos+1)), ((list !! (count)) !! (pos+1)), ((list !! (count+1)) !! pos), ((list !! (count+1)) !! (pos+1))]
                                   | count > 0 && count < (length list)-1 && pos == (length (list !! count))-1 = [((list !! (count-1)) !! (pos+1)), ((list !! (count-1)) !! pos), ((list !! (count)) !! (pos-1)), ((list !! (count+1)) !! pos), ((list !! (count+1)) !! (pos+1))]
                                   | count > 0 && count < (length list)-1 && pos < (length (list !! count))-1 && pos > 0 = [((list !! (count-1)) !! pos), ((list !! (count-1)) !! (pos+1)), ((list !! count) !! (pos+1)), ((list !! (count+1)) !! (pos+1)), ((list !! (count+1)) !! pos), ((list !! count) !! (pos-1))]

--dla linii parzystych, zaleznie od pozycji kropki i numeru wiersza sprawdzenie wszystkich mozliwych sasiadow 
checkEvenLine list count lng pos avbLets loop changes = if loop < 7 then 
    if count == 0 then
        if pos == 0 then do 
            let neighbours = checkNeighboursEven list count pos ++ checkNeighboursEven list count (pos+1) ++
                             checkNeighboursOdd list (count+1) pos ++ checkNeighboursOdd list (count+1) (pos+1)
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
        else if pos == ((length (list !! count))-1) then do 
            let neighbours = checkNeighboursEven list count pos ++ checkNeighboursEven list count (pos-1) ++
                             checkNeighboursOdd list (count+1) pos ++ checkNeighboursOdd list (count+1) (pos+1)
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
        else do 
            let neighbours = checkNeighboursEven list count (pos-1) ++ checkNeighboursEven list count pos ++ checkNeighboursEven list count (pos+1) ++
                             checkNeighboursOdd list (count+1) pos ++ checkNeighboursOdd list (count+1) (pos+1)
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
    else if count == (lng-1) then
        if pos == 0 then do 
            let neighbours = checkNeighboursEven list count pos ++ checkNeighboursEven list count (pos+1) ++
                             checkNeighboursOdd list (count-1) pos ++ checkNeighboursOdd list (count-1) (pos+1)
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
        else if pos == ((length (list !! count))-1) then do 
            let neighbours = checkNeighboursEven list count (pos-1) ++ checkNeighboursEven list count pos ++
                             checkNeighboursOdd list (count-1) pos ++ checkNeighboursOdd list (count-1) (pos+1)
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
        else do 
            let neighbours = checkNeighboursEven list count (pos-1) ++ checkNeighboursEven list count pos ++ checkNeighboursEven list count (pos+1) ++
                             checkNeighboursOdd list (count-1) pos ++ checkNeighboursOdd list (count-1) (pos+1)
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
    else
        if pos == 0 then do 
            let neighbours = checkNeighboursEven list count pos ++ checkNeighboursEven list count (pos+1) ++
                             checkNeighboursOdd list (count-1) (pos+1) ++ checkNeighboursOdd list (count-1) pos ++
                             checkNeighboursOdd list (count+1) (pos+1) ++ checkNeighboursOdd list (count+1) pos 
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
        else if pos == ((length (list !! count))-1) then do 
            let neighbours = checkNeighboursEven list count pos ++ checkNeighboursEven list count (pos-1) ++
                             checkNeighboursOdd list (count-1) (pos+1) ++ checkNeighboursOdd list (count-1) pos ++
                             checkNeighboursOdd list (count+1) (pos+1) ++ checkNeighboursOdd list (count+1) pos
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
        else do 
            let neighbours = checkNeighboursEven list count (pos-1) ++ checkNeighboursEven list count pos ++ checkNeighboursEven list count (pos+1) ++
                             checkNeighboursOdd list (count-1) (pos+1) ++ checkNeighboursOdd list (count-1) pos ++
                             checkNeighboursOdd list (count+1) (pos+1) ++ checkNeighboursOdd list (count+1) pos
            if (avbLets !! loop) `elem` neighbours then
                checkEvenLine list count lng pos avbLets (loop+1) changes    
            else
                putLetter list count lng pos avbLets loop changes
  else do
      print "No letters available"
      goBack list count lng pos avbLets loop changes 

--funkcja do wymiany n-tego znaku na liscie
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

--funkcja do znajdywania indeksow elementow na liscie
findPos list elt = [index | (index, e) <- zip [0..] list, e == elt]

--NIE WYKORZYSTYWANE
go :: IO ()
go = do --print "Podaj nazwe pliku z rozszerzeniem"
        --path <- getLine
        file <- (readFile "1.txt")
        print file
        let list = consume file 
        print list
        let content = (wordsWhen (==',') list) 
        --solve content
        let first = read (content !! 0) :: String
        print first
        print (content !! 0)
        print (content !! 1)
        print (content !! 2)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      ", " -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

consume :: [Char] -> [Char]
consume []       = []
consume ('[':xs) = consume' xs
consume (_  :xs) = consume xs

consume' :: [Char] -> [Char]
consume' []       = [] 
consume' (']':_) = []
consume' (x  :xs) = x : consume' xs