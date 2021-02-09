import qualified Text.Printf as Text (printf)
import qualified Data.Maybe as Maybe (fromJust)

main :: IO ()
main = do
    let list1 = tail [1]
    let list2 = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    let list3 = [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5]

    putStrLn "#11 run-length encoding of a list(modified)"
    Text.printf "%s -> %s\n" (show list1) (show $ encodeModified list1)
    Text.printf "%s -> %s\n" (show list2) (show $ encodeModified list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ encodeModified list3)

    putStrLn "#12 decode a run-length encoded list"
    Text.printf "%s -> %s\n" (show $ encodeModified list1) (show $ (decodeModified . encodeModified) list1)
    Text.printf "%s -> %s\n" (show $ encodeModified list2) (show $ (decodeModified . encodeModified) list2)
    Text.printf "%s -> %s\n\n" (show $ encodeModified list3) (show $ (decodeModified . encodeModified) list3)

    putStrLn "#13 run-length encoding of a list(direct solution)"
    Text.printf "%s -> %s\n" (show list1) (show $ encodeDirect list1)
    Text.printf "%s -> %s\n" (show list2) (show $ encodeDirect list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ encodeDirect list3)

    putStrLn "#14 duplicate the elements of a list"
    Text.printf "%s -> %s\n" (show list1) (show $ dupli list1)
    Text.printf "%s -> %s\n" (show list2) (show $ dupli list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ dupli list3)

    putStrLn "#15 replicate the elements of a list a given number of times"
    Text.printf "%s, %s -> %s\n" (show list1) (show 3) (show $ repli list1 3)
    Text.printf "%s, %s -> %s\n" (show list2) (show 3) (show $ repli list2 3)
    Text.printf "%s, %s -> %s\n\n" (show list3) (show 3) (show $ repli list3 3)

    putStrLn "#16 drop every N'th element from a list"
    Text.printf "%s, %s -> %s\n" (show list1) (show 5) (show $ myDrop list1 5)
    Text.printf "%s, %s -> %s\n" (show list2) (show 5) (show $ myDrop list2 5)
    Text.printf "%s, %s -> %s\n\n" (show list3) (show 5) (show $ myDrop list3 5)

    putStrLn "#17 split a list into two parts; the length of the first part is given"
    Text.printf "%s, %s -> %s\n" (show list1) (show 5) (show $ split list1 5)
    Text.printf "%s, %s -> %s\n" (show list2) (show 5) (show $ split list2 5)
    Text.printf "%s, %s -> %s\n\n" (show list3) (show 5) (show $ split list3 5)

    putStrLn "#18 Extract a slice from a list"
    Text.printf "%s, %s, %s -> %s\n" (show list1) (show 3) (show 8) (show $ slice list1 3 8)
    Text.printf "%s, %s, %s -> %s\n" (show list2) (show 3) (show 8) (show $ slice list2 3 8)
    Text.printf "%s, %s, %s -> %s\n\n" (show list3) (show 3) (show 8) (show $ slice list3 3 8)

    putStrLn "#19 rotate a list N places to the left"
    Text.printf "%s, %s -> %s\n" (show list1) (show 3) (show $ rotate list1 3)
    Text.printf "%s, %s -> %s\n" (show list2) (show 3) (show $ rotate list2 3)
    Text.printf "%s, %s -> %s\n" (show list3) (show (-3)) (show $ rotate list3 (-3))
    Text.printf "%s, %s -> %s\n\n" (show list3) (show 3) (show $ rotate list3 3)

    putStrLn "#20 remove the K'th element from a list"
    Text.printf "%s, %s -> (%s, %s)\n" (show list1) (show 5) (show $ fst $ removeAt list1 5)
        (show $ snd $ removeAt list1 5)
    Text.printf "%s, %s -> (%s, %s)\n" (show list2) (show 5) (show $ fst $ removeAt list2 5)
        (show $ Maybe.fromJust $ snd $ removeAt list2 5)
    Text.printf "%s, %s -> (%s, %s)\n\n" (show list3) (show 5) (show $ fst $ removeAt list3 5)
        (show $ Maybe.fromJust $ snd $ removeAt list3 5)

pack :: Eq a => [a] -> [[a]]
pack list = pack' list [] []
    where pack' :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
          pack' list packedSublist packedList = case list of
            [] -> case packedSublist of
                [] -> packedList
                _ -> packedList ++ [packedSublist]
            (element : rest) -> case packedSublist of
                [] -> pack' rest (element : packedSublist) packedList
                (duplicateElement : _)
                    | element == duplicateElement -> pack' rest (element : packedSublist) packedList
                    | otherwise -> pack' rest [element] (packedList ++ [packedSublist])

data EncodedElement a = Element a | Pair (Int, a)

instance (Show a) => Show (EncodedElement a) where
  show (Element element) = show element
  show (Pair (length, element)) = "(" ++ show length ++ ", " ++ show element ++ ")"

encodeModified :: Eq a => [a] -> [EncodedElement a]
encodeModified list = encodeModified' (pack list) []
    where encodeModified' :: Eq a => [[a]] -> [EncodedElement a] -> [EncodedElement a]
          encodeModified' packedList encodedList = case packedList of
            [] -> encodedList
            (packedSublist : packedRest) -> case packedSublist of
                (subListElement : _) ->
                  if length packedSublist > 1
                  then encodeModified' packedRest (encodedList ++ [Pair (length packedSublist, subListElement)])
                  else encodeModified' packedRest (encodedList ++ [Element subListElement])

repeatElement :: Int -> a -> [a]
repeatElement numberOfReplications element = repeatElement' numberOfReplications element []
        where repeatElement' :: Int -> a -> [a] -> [a]
              repeatElement' numberOfReplications element listWithRepitions
                | numberOfReplications <= 0 = listWithRepitions
                | otherwise = repeatElement' (numberOfReplications - 1) element (element : listWithRepitions)

decodeModified :: [EncodedElement a] -> [a]
decodeModified encodedList = decodeModified' encodedList []
    where decodeModified' :: [EncodedElement a] -> [a] -> [a]
          decodeModified' encodedList decodedList = case encodedList of
            [] -> decodedList
            (Element element : encodedRest) -> decodeModified' encodedRest (decodedList ++ [element])
            (Pair (numberOfReplications, element) : encodedRest) ->
                decodeModified' encodedRest (decodedList ++ repeatElement numberOfReplications element)

encodeDirect :: Eq a => [a] -> [EncodedElement a]
encodeDirect list = encodeDirect' list []
    where encodeDirect' :: Eq a => [a] -> [EncodedElement a] -> [EncodedElement a]
          encodeDirect' list encodedList = case list of
            [] -> encodedList
            (element : rest) ->
                if runLength > 1
                then encodeDirect' reducedRest (encodedList ++ [Pair(runLength, element)])
                else encodeDirect' rest (encodedList ++ [Element element])
                where (runLength, reducedRest) = countAndRemoveConsecutiveDuplicates element 1 rest
                      countAndRemoveConsecutiveDuplicates :: Eq a => a -> Int -> [a] -> (Int, [a])
                      countAndRemoveConsecutiveDuplicates element runLength subList = case subList of
                          [] -> (runLength, subList)
                          (subElement : rest) ->
                            if element == subElement
                            then countAndRemoveConsecutiveDuplicates element (runLength + 1) rest
                            else (runLength, subList)

dupli :: [a] -> [a]
dupli list = dupli' list []
    where dupli' :: [a] -> [a] -> [a]
          dupli' list duplicatedList = case list of
            [] -> reverse duplicatedList
            (element : rest) -> dupli' rest (element : element : duplicatedList)

repli :: [a] -> Int -> [a]
repli list numberOfReplications = repli' list numberOfReplications []
    where repli' :: [a] -> Int -> [a] -> [a]
          repli' list numberOfReplications replicatedList = case list of
            [] -> replicatedList
            (element : rest) ->
                repli' rest numberOfReplications (replicatedList ++ repeatElement numberOfReplications element)

myDrop :: [a] -> Int -> [a]
myDrop list number = myDrop' list number 1 []
        where myDrop' :: [a] -> Int -> Int -> [a] -> [a]
              myDrop' list number count reducedList = case list of
                [] -> reverse reducedList
                (element : rest)
                    | count == number -> myDrop' rest number 1 reducedList
                    | otherwise -> myDrop' rest number (count + 1) (element : reducedList)

split :: [a] -> Int -> ([a], [a])
split list lengthOfFirstPart = split' list lengthOfFirstPart 0 ([], [])
    where split' :: [a] -> Int -> Int -> ([a], [a]) -> ([a], [a])
          split' list lengthOfFirstPart index (firstPart, secondPart) = case list of
            [] -> (reverse firstPart, reverse secondPart)
            (element : rest)
                | index < lengthOfFirstPart -> split' rest lengthOfFirstPart (index + 1) (element : firstPart, secondPart)
                | otherwise -> split' rest lengthOfFirstPart (index + 1) (firstPart, element : secondPart)

slice :: [a] -> Int -> Int -> [a]
slice list lowerBound higherBound = slice' list lowerBound higherBound 1 []
    where slice' :: [a] -> Int -> Int -> Int -> [a] -> [a]
          slice' list lowerBound higherBound index slicedList = case list of
            [] -> reverse slicedList
            (element : rest) ->
                if index >= lowerBound && index <= higherBound
                then slice' rest lowerBound higherBound (index + 1) (element : slicedList)
                else slice' rest lowerBound higherBound (index + 1) slicedList

rotate :: Eq a => [a] -> Int -> [a]
rotate list positions
    | positions == 0 || null list = list
    | positions > 0 = snd (split list positions) ++ fst (split list positions)
    | otherwise = snd (split list (length list + positions)) ++ fst (split list (length list + positions))

removeAt :: [a] -> Int -> ([a], Maybe a)
removeAt list position = removeAt' list position 1 []
    where removeAt' :: [a] -> Int -> Int -> [a] -> ([a], Maybe a)
          removeAt' list position index resultList = case list of
            [] -> (reverse resultList, Nothing)
            (element : rest)
                | position == index -> (resultList ++ rest, Just element)
                | otherwise -> removeAt' rest position (index + 1) (element : resultList)
