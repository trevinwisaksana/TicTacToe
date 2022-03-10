import Data.Char
import Control.Monad.Trans.Maybe

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

enterPosition :: Int -> [(Int, Row)] -> String -> IO()
enterPosition count positions character = do
                                    putStrLn ""
                                    generateTable positions
                                    putStrLn ""
                                    newPosition <- runMaybeT $ do
                                        positionEntered <- askPosition positions
                                        positionCoordinate <- parsePosition positionEntered
                                        verifiedPosition <- getPositionIfNotTaken positionCoordinate positions
                                        addPosition verifiedPosition character positions
                                    case newPosition of
                                        Nothing -> enterPosition count positions character
                                        Just value -> do
                                            let hasWinner = checkForWinner character value
                                            if hasWinner 
                                                then showWinner character
                                                else handleIfDraw count value character

-- Asking Position

askPosition :: [(Int, Row)] -> MaybeT IO String
askPosition positions = MaybeT $ do
    putStrLn "Enter coordinates (e.g. A1, B2): "
    positionChosen <- getLine
    let inputValid = isPositionValid positionChosen
    if inputValid
        then return $ Just positionChosen
        else do
            putStrLn "\nERROR: Please enter a valid coordinate e.g. A1, C3"
            return Nothing

isCoordinateValid :: String -> Bool
isCoordinateValid "" = False
isCoordinateValid coordinates = (length coordinates == 2) && (do
                                    let hasCorrectAlphabet = head coordinates `elem` ['A', 'B', 'C']
                                    let hasCorrectNumber = last coordinates `elem` ['1', '2', '3']
                                    hasCorrectAlphabet && hasCorrectNumber)

isPositionValid :: String -> Bool
isPositionValid "" = False
isPositionValid coordinates = (length coordinates == 2) && (do
                                    let hasCorrectAlphabet = head coordinates `elem` ['A', 'B', 'C']
                                    let hasCorrectNumber = last coordinates `elem` ['1', '2', '3']
                                    hasCorrectAlphabet && hasCorrectNumber)

getPositionIfNotTaken :: (Int, Int) -> [(Int, Row)] -> MaybeT IO (Int, Int)
getPositionIfNotTaken coordinates positions = MaybeT $ do
    let characterAtRow = getValueInRow coordinates positions
    case characterAtRow of
        Just value -> do
            if value == "x" || value == "o"
                then do 
                    putStrLn "\nERROR: Position taken. Please enter a valid coordinate e.g. A2, C3"
                    return Nothing
                else return $ Just coordinates
        Nothing -> do
            putStrLn "\nERROR: Position taken. Please enter a valid coordinate e.g. A2, C3"
            return Nothing

getValueInRow :: (Int, Int) -> [(Int, Row)] -> Maybe String
getValueInRow coordinates positions
                                    | xValue == 1 = Just (a $ findRow yValue positions)
                                    | xValue == 2 = Just (b $ findRow yValue positions)
                                    | xValue == 3 = Just (c $ findRow yValue positions)
                                    | otherwise = Nothing
                                    where
                                        xValue = fst coordinates
                                        yValue = snd coordinates

parsePosition :: String -> MaybeT IO (Int, Int)
parsePosition position = MaybeT $ do
    let xValue = alphabetToIndex $ head position
    case xValue of
        Just value -> return $ Just (value, digitToInt $ last position)
        Nothing -> do
            putStrLn "\nERROR: Failed to parse position. Please enter a valid coordinate e.g. A2, C3"
            return Nothing

alphabetToIndex :: Char -> Maybe Int
alphabetToIndex alphabet
                         | alphabet == 'A' = Just 1
                         | alphabet == 'B' = Just 2
                         | alphabet == 'C' = Just 3
                         | otherwise = Nothing

-- Updating Table

addPosition :: (Int, Int) -> String -> [(Int, Row)] -> MaybeT IO [(Int, Row)]
addPosition position character database = MaybeT $ do
    let updatedDatabase = updateDatabase position character database
    generateTable updatedDatabase
    return $ Just updatedDatabase

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

-- Miscelaneous

getCharacterName :: String -> Maybe String
getCharacterName alphabet
                         | alphabet == "o" = Just "Noughts (o)"
                         | alphabet == "x" = Just "Crosses (x)"
                         | otherwise = Nothing

switchCharacter :: String -> String
switchCharacter current
                      | current == "o" = "x"
                      | current == "x" = "o"
                      | otherwise = current

-- Ending Game

isGameDraw :: Int -> Bool
isGameDraw count = count == 8

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

handleIfDraw :: Int -> [(Int, Row)] -> String -> IO()
handleIfDraw count positions character = do
    let isDraw = isGameDraw count
    if isDraw
        then do
            putStrLn "\nIt's a draw!"
            return ()
        else do
            reenterPosition count positions character

reenterPosition :: Int -> [(Int, Row)] -> String -> IO()
reenterPosition count positions character = do
    let newCharacter = switchCharacter character
    let characterName = getCharacterName newCharacter
    case characterName of
      Nothing -> putStrLn "\nERROR: Unidentified character"
      Just value -> do 
          putStrLn ("\nYour move " ++ value)
          let newCount = count + 1
          enterPosition newCount positions newCharacter
    
showWinner :: String -> IO()
showWinner character = do
    let characterName = getCharacterName character
    case characterName of
      Nothing -> putStrLn "\nERROR: Failed to show winner"
      Just value -> do
        putStrLn ("\n" ++ value ++ " is the winner!")
        return ()

-- Run

runGame :: [(Int, Row)] -> String -> IO()
runGame positions character = do
                              putStrLn "\nLet the games begin!"
                              let characterName = getCharacterName character
                              case characterName of
                                Nothing -> putStrLn "\nERROR: Unidentified character"
                                Just value -> do
                                    putStrLn ("\nYour move " ++ value)
                                    enterPosition 0 positions character

main :: IO ()
main = do
        displayTutorial
        characterChosen <- chooseCharacter
        runGame database characterChosen
