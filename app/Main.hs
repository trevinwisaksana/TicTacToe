import Data.Char

-- TIC TAC TOE
-- Create a game that replicates TIC TAC TOE that involves getting user input
-- 1. When the script runs, it shows a set of instructions and requires the player to choose 'X' or 'O' to start
-- 1. When the script runs, it shows a set of instructions and requires pressing `s` to start
-- 2. When the game starts, it shows an empty grid and the player can type the coordinates to select the position
-- 3. Each time the player chooses a location the Player 2 would get the next turn

data Row = Row { a :: String, b :: String, c :: String} deriving (Show)

database :: [(Int, Row)]
database = [(1, Row { a = "_", b = "_", c = "_" }),
            (2, Row { a = "_", b = "_", c = "_" }),
            (3, Row { a = " ", b = " ", c = " " })
           ]

-- winnerSampleDatabase :: [(Int, Row)]
-- winnerSampleDatabase = [(1, Row { a = "x", b = "x", c = "x" }),
--                         (2, Row { a = "_", b = "_", c = "_" }),
--                         (3, Row { a = " ", b = " ", c = " " })
--                        ]

generateTable :: [(Int, Row)] -> IO()
generateTable [] = do
                     putStrLn "   A   B   C "
                     putStrLn "1 ___|___|___"
                     putStrLn "2 ___|___|___"
                     putStrLn "3    |   |   "
generateTable positions = do
                          putStrLn "   A   B   C "
                          putStrLn ("1 _" ++ a1 ++ "_|_" ++ b1 ++ "_|_" ++ c1 ++ "_")
                          putStrLn ("2 _" ++ a2 ++ "_|_" ++ b2 ++ "_|_" ++ c2 ++ "_")
                          putStrLn ("3  " ++ a3 ++ " | " ++ b3 ++ " | " ++ c3 ++ " ")
                          where
                              a1 = a $ findRow 1 positions
                              a2 = a $ findRow 2 positions
                              a3 = a $ findRow 3 positions
                              b1 = b $ findRow 1 positions
                              b2 = b $ findRow 2 positions
                              b3 = b $ findRow 3 positions
                              c1 = c $ findRow 1 positions
                              c2 = c $ findRow 2 positions
                              c3 = c $ findRow 3 positions

findRow :: Int -> [(Int, Row)] -> Row
findRow index database = do
                         let row = lookup index database
                         case row of
                            Just row -> row
                            Nothing -> error "Row cannot be found"

displayTutorial :: IO()
displayTutorial = do
                  putStrLn "Welcome to a game of TIC TAC TOE!"
                  putStrLn ""
                  putStrLn "Here's a quick tip, to select a location, type the coordinates e.g. B2, C3, A1, etc."
                  putStrLn ""
                  generateTable []
                  putStrLn ""

chooseCharacter :: IO String
chooseCharacter = do
                    putStrLn "Please choose your character: X or O (x/o)"
                    characterSelected <- getLine
                    case characterSelected of
                        "o" -> do
                            putStrLn "You have chosen Noughts (O)"
                            return characterSelected
                        "x" -> do
                            putStrLn "You have chosen Crosses (X)"
                            return characterSelected
                        _ -> do
                            chooseCharacter

runGame :: [(Int, Row)] -> String -> IO()
runGame positions character = do
                              putStrLn ""
                              putStrLn "Let the games begin!"
                              let characterName = getFullCharacterName character
                              putStrLn ("Your move " ++ characterName)
                              enterPosition 0 positions character

enterPosition :: Int -> [(Int, Row)] -> String -> IO()
enterPosition count positions character = do
                                    putStrLn ""
                                    generateTable positions
                                    putStrLn ""
                                    putStr "Enter coordinates: "
                                    positionChosen <- getLine
                                    let coordinateValid = isCoordinateValid positionChosen
                                    let isTaken = isPositionTaken positionChosen positions
                                    if coordinateValid && not isTaken
                                        then do
                                            let newPosition = addPosition positionChosen character positions
                                            case newPosition of
                                                Just value -> do
                                                    generateTable value
                                                    let hasWinner = checkForWinner character value
                                                    if hasWinner
                                                        then do
                                                            let characterName = getFullCharacterName character
                                                            putStrLn ""
                                                            putStrLn (characterName ++ " is the winner!")
                                                            return ()
                                                        else do
                                                            if count == 8
                                                                then do
                                                                    putStrLn ""
                                                                    putStrLn "It's a draw!"
                                                                    return ()
                                                                else do
                                                                    let newCharacter = switchCharacter character
                                                                    let characterName = getFullCharacterName newCharacter
                                                                    putStrLn ""
                                                                    putStrLn ("Your move " ++ characterName)
                                                                    let newCount = count + 1
                                                                    enterPosition newCount value newCharacter
                                                Nothing -> do
                                                    putStrLn ""
                                                    putStrLn "ERROR: Please enter a valid coordinate e.g. A2, C3"
                                                    enterPosition count positions character
                                        else do
                                            putStrLn ""
                                            putStrLn "ERROR: Please enter a valid coordinate e.g. A2, C3"
                                            enterPosition count positions character

isCoordinateValid :: String -> Bool
isCoordinateValid "" = False
isCoordinateValid coordinates = (length coordinates == 2) && (do
                                    let hasCorrectAlphabet = head coordinates `elem` ['A', 'B', 'C']
                                    let hasCorrectNumber = last coordinates `elem` ['1', '2', '3']
                                    hasCorrectAlphabet && hasCorrectNumber)

switchCharacter :: String -> String
switchCharacter current
                      | current == "o" = "x"
                      | current == "x" = "o"
                      | otherwise = current

isPositionTaken :: String -> [(Int, Row)] -> Bool
isPositionTaken coordinates positions = do
                          let target = parsePosition coordinates
                          case target of
                              Just value -> do
                                  let characterAtRow = getValueInRow value positions
                                  case characterAtRow of
                                    Just value -> value == "x" || value == "o"
                                    Nothing -> False
                              Nothing -> False

checkForWinner :: String -> [(Int, Row)] -> Bool
checkForWinner character positions
                                  | all (==character) [a1, a2, a3] = True
                                  | all (==character) [b1, b2, b3] = True
                                  | all (==character) [c1, c2, c3] = True
                                  | all (==character) [a1, b1, c1] = True
                                  | all (==character) [a2, b2, c2] = True
                                  | all (==character) [a3, b3, c3] = True
                                  | all (==character) [a1, b2, c3] = True
                                  | all (==character) [a3, b2, c1] = True
                                  | otherwise = False
                                  where
                                    a1 = a $ findRow 1 positions
                                    a2 = a $ findRow 2 positions
                                    a3 = a $ findRow 3 positions
                                    b1 = b $ findRow 1 positions
                                    b2 = b $ findRow 2 positions
                                    b3 = b $ findRow 3 positions
                                    c1 = c $ findRow 1 positions
                                    c2 = c $ findRow 2 positions
                                    c3 = c $ findRow 3 positions

addPosition :: String -> String -> [(Int, Row)] -> Maybe [(Int, Row)]
addPosition position character database = do
                                 let target = parsePosition position
                                 case target of
                                     Just value -> do
                                         Just $ updateDatabase value character database
                                     Nothing -> Nothing

updateDatabase :: (Int, Int) -> String -> [(Int, Row)] -> [(Int, Row)]
updateDatabase coordinates character = map modify
    where modify (yValue, row)
            | yValue == snd coordinates = do
                                          let updatedRow = updateRow (fst coordinates) character row
                                          (snd coordinates, updatedRow)
            | otherwise = (yValue, row)

updateRow :: Int -> String -> Row -> Row
updateRow index character row
                              | index == 1 = row { a = character }
                              | index == 2 = row { b = character }
                              | index == 3 = row { c = character }
                              | otherwise = row

getValueInRow :: (Int, Int) -> [(Int, Row)] -> Maybe String
getValueInRow coordinates positions
                                    | xValue == 1 = Just (a $ findRow yValue positions)
                                    | xValue == 2 = Just (b $ findRow yValue positions)
                                    | xValue == 3 = Just (c $ findRow yValue positions)
                                    | otherwise = Nothing
                                    where
                                        xValue = fst coordinates
                                        yValue = snd coordinates

parsePosition :: String -> Maybe (Int, Int)
parsePosition "" = Nothing
parsePosition position = do
                        let xValue = alphabetToIndex $ head position
                        case xValue of
                            Just value -> do Just (value, digitToInt $ last position)
                            Nothing -> Nothing

alphabetToIndex :: Char -> Maybe Int
alphabetToIndex alphabet
                         | alphabet == 'A' = Just 1
                         | alphabet == 'B' = Just 2
                         | alphabet == 'C' = Just 3
                         | otherwise = Nothing

getFullCharacterName :: String  -> String
getFullCharacterName alphabet
                         | alphabet == "o" = "Noughts (o)"
                         | alphabet == "x" = "Crosses (x)"
                         | otherwise = ""

main :: IO ()
main = do
        displayTutorial
        characterChosen <- chooseCharacter
        runGame database characterChosen
