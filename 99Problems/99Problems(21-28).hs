import qualified Text.Printf as Text (printf)
import qualified System.Random as Random (randomRIO)
import qualified Data.Maybe as Maybe (fromJust)

main :: IO ()
main = do
    let list1 = [1, 2, 3, 4]
    let list2 = ['a', 'b', 'c', 'd']
    let list3 = tail [1]

    putStrLn "#21 insert an element at a given position into a list"
    Text.printf "%s, %s, %s -> %s\n" (show list1) (show 5) (show 3) (show $ insertAt list1 5 3)
    Text.printf "%s, %s, %s -> %s\n" (show list2) (show 'e') (show 3) (show $ insertAt list2 'e' 3)
    Text.printf "%s, %s, %s -> %s\n\n" (show list3) (show 5) (show 3) (show $ insertAt list3 5 3)

    putStrLn "#22 create a list containing all integers within a given range"
    Text.printf "%s, %s -> %s\n" (show 3) (show 8) (show $ myRange 3 8)
    Text.printf "%s, %s -> %s\n" (show (-8)) (show (-3)) (show $ myRange (-8)(-3))
    Text.printf "%s, %s -> %s\n\n" (show 8) (show 3) (show $ myRange 8 3)

    putStrLn "#23 extract a given number of randomly selected elements from a list"
    result <- randomSelection list1 3
    Text.printf "%s, %s -> %s\n" (show list1) (show 3) (show $ result)
    result <- randomSelection list2 3
    Text.printf "%s, %s -> %s\n" (show list2) (show 3) (show $ result)
    result <- randomSelection list3 3
    Text.printf "%s, %s -> %s\n\n" (show list3) (show 3) (show $ result)

    putStrLn "#24 draw N different random numbers from the set 1..M"
    result <- lotto 6 49
    Text.printf "%s, %s -> %s\n" (show 6) (show 49) (show result)
    result <- lotto 0 49
    Text.printf "%s, %s -> %s\n" (show 0) (show 49) (show $ result)
    result <- lotto 6 0
    Text.printf "%s, %s -> %s\n" (show 6) (show 0) (show $ result)
    result <- lotto 49 6
    Text.printf "%s, %s -> %s\n\n" (show 49) (show 6) (show $ result)

    putStrLn "#25 generate a random permutation of the elements of a list"
    result <- randomPermutation list1
    Text.printf "%s -> %s\n" (show list1) (show result)
    result <- randomPermutation list2
    Text.printf "%s -> %s\n" (show list2) (show result)
    result <- randomPermutation list3
    Text.printf "%s -> %s\n\n" (show list3) (show result)

insertAt :: [a] -> a -> Int -> [a]
insertAt list valueToBeInserted position = insertAt' list valueToBeInserted position 1 []
    where insertAt' list valueToBeInserted position index updatedList = case list of
            [] -> reverse updatedList
            (element : rest) ->
                if index == position
                then insertAt' rest valueToBeInserted position (index + 1) (valueToBeInserted : element : updatedList)
                else insertAt' rest valueToBeInserted position (index + 1) (element : updatedList)

myRange :: Int -> Int -> [Int]
myRange lowerBound higherBound = myRange' lowerBound higherBound []
    where myRange' lowerBound higherBound resultList
            | lowerBound <= higherBound = myRange' (lowerBound + 1) higherBound (resultList ++ [lowerBound])
            | otherwise = resultList

removeAt :: [a] -> Int -> ([a], Maybe a)
removeAt list position = removeAt' list position 1 []
    where removeAt' list position index resultList = case list of
            [] -> (resultList, Nothing)
            (element : rest)
                | position == index -> ((resultList ++ rest), Just element)
                | otherwise -> removeAt' rest position (index + 1) (resultList ++ [element])

randomSelection :: [a] -> Int -> IO [a]
randomSelection list numberOfElements
    | length list == 0 || numberOfElements <= 0 = return []
    | otherwise = do
        randomNumber <- Random.randomRIO (1, length list)
        let randomElement = (Maybe.fromJust $ snd $ removeAt list randomNumber)
        resultList <- randomSelection (fst $ removeAt list randomNumber) (numberOfElements - 1)
        return (randomElement : resultList)

lotto :: Int -> Int -> IO [Int]
lotto numberOfElements higherBound
    | higherBound < numberOfElements || higherBound <= 0 = return []
    | otherwise = randomSelection (myRange 1 higherBound) numberOfElements

randomPermutation :: [a] -> IO [a]
randomPermutation list = randomSelection list (length list)
