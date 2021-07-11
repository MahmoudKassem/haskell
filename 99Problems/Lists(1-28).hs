{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Text.Printf as Text (printf)
import qualified System.Random as Random (randomRIO)
import qualified Data.Maybe as Maybe (fromJust)

main :: IO ()
main = do
    let list1 = [1, 2, 3, 4]
    let list2 = ['a', 'b', 'c', 'd']
    let list3 = [] :: [Integer]
    let list4 = [1]
    let list5 = [1, 3, 1]
    let list6 = ['a', 'b', 'c', 'c', 'b', 'a']
    let list7 = List [Element 1, Element 2, List [Element 3, List [Element 4, Element 5]], Element 6]
    let list8 = List [Element 'a', Element 'b', List [Element 'c', List [Element 'd', Element 'e']], Element 'f']
    let list9 = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    let list10 = [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5]

    putStrLn "#1 last element of a list"
    Text.printf "%s -> %s\n" (show list1) (show $ Maybe.fromJust $ last_ list1)
    Text.printf "%s -> %s\n" (show list2) (show $ Maybe.fromJust $ last_ list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ last_ list3)

    putStrLn "#2 last but one element of a list"
    Text.printf "%s -> %s\n" (show list1) (show $ Maybe.fromJust $ lastButOne list1)
    Text.printf "%s -> %s\n" (show list2) (show $ Maybe.fromJust $ lastButOne list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ lastButOne list3)

    putStrLn "#3 k'th element of a list"
    Text.printf "%s, %s -> %s\n" (show list1) (show 2) (show $ Maybe.fromJust $ elementAt list1 2)
    Text.printf "%s, %s -> %s\n" (show list2) (show 2) (show $ Maybe.fromJust $ elementAt list2 2)
    Text.printf "%s, %s -> %s\n\n" (show list3) (show 2) (show $ elementAt list3 2)

    putStrLn "#4 number of elements in a list"
    Text.printf "%s -> %s\n" (show list1) (show $ length_ list1)
    Text.printf "%s -> %s\n" (show list2) (show $ length_ list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ length_ list3)

    putStrLn "#5 reverse a list"
    Text.printf "%s -> %s\n" (show list1) (show $ reverse_ list1)
    Text.printf "%s -> %s\n" (show list2) (show $ reverse_ list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ reverse_ list3)

    putStrLn "#6 is list a palindrome"
    Text.printf "%s -> %s\n" (show list1) (show $ isPalindrom list1)
    Text.printf "%s -> %s\n" (show list2) (show $ isPalindrom list2)
    Text.printf "%s -> %s\n" (show list3) (show $ isPalindrom list3)
    Text.printf "%s -> %s\n" (show list4) (show $ isPalindrom list4)
    Text.printf "%s -> %s\n" (show list5) (show $ isPalindrom list5)
    Text.printf "%s -> %s\n\n" (show list6) (show $ isPalindrom list6)

    putStrLn "#7 flatten a nested list"
    Text.printf "%s -> %s\n" (show list7) (show $ flatten list7)
    Text.printf "%s -> %s\n\n" (show list8) (show $ flatten list8)

    putStrLn "#8 remove consecutive duplicates in a list"
    Text.printf "%s -> %s\n" (show list1) (show $ compress list1)
    Text.printf "%s -> %s\n" (show list2) (show $ compress list2)
    Text.printf "%s -> %s\n" (show list3) (show $ compress list3)
    Text.printf "%s -> %s\n" (show list9) (show $ compress list9)
    Text.printf "%s -> %s\n\n" (show list10) (show $ compress list10)

    putStrLn "#9 pack consecutive duplicates of elements into sublists"
    Text.printf "%s -> %s\n" (show list1) (show $ pack list1)
    Text.printf "%s -> %s\n" (show list2) (show $ pack list2)
    Text.printf "%s -> %s\n" (show list3) (show $ pack list3)
    Text.printf "%s -> %s\n" (show list9) (show $ pack list9)
    Text.printf "%s -> %s\n\n" (show list10) (show $ pack list10)

    putStrLn "#10 run-length encoding of a list"
    Text.printf "%s -> %s\n" (show list1) (show $ encode list1)
    Text.printf "%s -> %s\n" (show list2) (show $ encode list2)
    Text.printf "%s -> %s\n" (show list3) (show $ encode list3)
    Text.printf "%s -> %s\n" (show list9) (show $ encode list9)
    Text.printf "%s -> %s\n\n" (show list10) (show $ encode list10)

    putStrLn "#11 run-length encoding of a list(modified)"
    Text.printf "%s -> %s\n" (show list3) (show $ encodeModified list3)
    Text.printf "%s -> %s\n" (show list9) (show $ encodeModified list9)
    Text.printf "%s -> %s\n\n" (show list10) (show $ encodeModified list10)

    putStrLn "#12 decode a run-length encoded list"
    Text.printf "%s -> %s\n" (show $ encodeModified list3) (show $ (decodeModified . encodeModified) list3)
    Text.printf "%s -> %s\n" (show $ encodeModified list9) (show $ (decodeModified . encodeModified) list9)
    Text.printf "%s -> %s\n\n" (show $ encodeModified list10) (show $ (decodeModified . encodeModified) list10)

    putStrLn "#13 run-length encoding of a list(direct solution)"
    Text.printf "%s -> %s\n" (show list3) (show $ encodeDirect list3)
    Text.printf "%s -> %s\n" (show list9) (show $ encodeDirect list9)
    Text.printf "%s -> %s\n\n" (show list10) (show $ encodeDirect list10)

    putStrLn "#14 duplicate the elements of a list"
    Text.printf "%s -> %s\n" (show list3) (show $ dupli list3)
    Text.printf "%s -> %s\n" (show list9) (show $ dupli list9)
    Text.printf "%s -> %s\n\n" (show list10) (show $ dupli list10)

    putStrLn "#15 replicate the elements of a list a given number of times"
    Text.printf "%s, %s -> %s\n" (show list3) (show 3) (show $ repli list3 3)
    Text.printf "%s, %s -> %s\n" (show list9) (show 3) (show $ repli list9 3)
    Text.printf "%s, %s -> %s\n\n" (show list10) (show 3) (show $ repli list10 3)

    putStrLn "#16 drop every N'th element from a list"
    Text.printf "%s, %s -> %s\n" (show list3) (show 5) (show $ drop_ list3 5)
    Text.printf "%s, %s -> %s\n" (show list9) (show 5) (show $ drop_ list9 5)
    Text.printf "%s, %s -> %s\n\n" (show list10) (show 5) (show $ drop_ list10 5)

    putStrLn "#17 split a list into two parts; the length of the first part is given"
    Text.printf "%s, %s -> %s\n" (show list3) (show 5) (show $ split list3 5)
    Text.printf "%s, %s -> %s\n" (show list9) (show 5) (show $ split list9 5)
    Text.printf "%s, %s -> %s\n\n" (show list10) (show 5) (show $ split list10 5)

    putStrLn "#18 Extract a slice from a list"
    Text.printf "%s, %s, %s -> %s\n" (show list3) (show 3) (show 8) (show $ slice list3 3 8)
    Text.printf "%s, %s, %s -> %s\n" (show list9) (show 3) (show 8) (show $ slice list9 3 8)
    Text.printf "%s, %s, %s -> %s\n\n" (show list10) (show 3) (show 8) (show $ slice list10 3 8)

    putStrLn "#19 rotate a list N places to the left"
    Text.printf "%s, %s -> %s\n" (show list3) (show 3) (show $ rotate list3 3)
    Text.printf "%s, %s -> %s\n" (show list9) (show 3) (show $ rotate list9 3)
    Text.printf "%s, %s -> %s\n" (show list10) (show (-3)) (show $ rotate list10 (-3))
    Text.printf "%s, %s -> %s\n\n" (show list10) (show 3) (show $ rotate list10 3)

    putStrLn "#20 remove the K'th element from a list"
    Text.printf "%s, %s -> (%s, %s)\n" (show list3) (show 5) (show $ fst $ removeAt list3 5) (show $ snd $ removeAt list3 5)
    Text.printf "%s, %s -> (%s, %s)\n" (show list9) (show 5) (show $ fst $ removeAt list9 5) (show $ Maybe.fromJust $ snd $ removeAt list9 5)
    Text.printf "%s, %s -> (%s, %s)\n\n" (show list10) (show 5) (show $ fst $ removeAt list10 5) (show $ Maybe.fromJust $ snd $ removeAt list10 5)

    putStrLn "#21 insert an element at a given position into a list"
    Text.printf "%s, %s, %s -> %s\n" (show list1) (show 5) (show 3) (show $ insertAt list1 5 3)
    Text.printf "%s, %s, %s -> %s\n" (show list2) (show 'e') (show 3) (show $ insertAt list2 'e' 3)
    Text.printf "%s, %s, %s -> %s\n\n" (show list3) (show 5) (show 3) (show $ insertAt list3 5 3)

    putStrLn "#22 create a list containing all integers within a given range"
    Text.printf "%s, %s -> %s\n" (show 3) (show 8) (show $ range 3 8)
    Text.printf "%s, %s -> %s\n" (show (-8)) (show (-3)) (show $ range (-8)(-3))
    Text.printf "%s, %s -> %s\n\n" (show 8) (show 3) (show $ range 8 3)

    putStrLn "#23 extract a given number of randomly selected elements from a list"
    result <- randomSelection list1 3
    Text.printf "%s, %s -> %s\n" (show list1) (show 3) (show result)
    result <- randomSelection list2 3
    Text.printf "%s, %s -> %s\n" (show list2) (show 3) (show result)
    result <- randomSelection list3 3
    Text.printf "%s, %s -> %s\n\n" (show list3) (show 3) (show result)

    putStrLn "#24 draw N different random numbers from the set 1..M"
    result <- lotto 6 49
    Text.printf "%s, %s -> %s\n" (show 6) (show 49) (show result)
    result <- lotto 0 49
    Text.printf "%s, %s -> %s\n" (show 0) (show 49) (show result)
    result <- lotto 6 0
    Text.printf "%s, %s -> %s\n" (show 6) (show 0) (show result)
    result <- lotto 49 6
    Text.printf "%s, %s -> %s\n\n" (show 49) (show 6) (show result)

    putStrLn "#25 generate a random permutation of the elements of a list"
    result <- randomPermutation list1
    Text.printf "%s -> %s\n" (show list1) (show result)
    result <- randomPermutation list2
    Text.printf "%s -> %s\n" (show list2) (show result)
    result <- randomPermutation list3
    Text.printf "%s -> %s\n\n" (show list3) (show result)

    putStrLn "#26 generate the combinations of K distinct objects chosen from the N elements of a list"
    Text.printf "%s, %s -> %s\n" (show list1) (show 3) (show $ combinations list1 3)
    Text.printf "%s, %s -> %s\n" (show list2) (show 3) (show $ combinations list2 3)
    Text.printf "%s, %s -> %s\n" (show list3) (show 3) (show $ combinations list3 3)
    Text.printf "%s, %s -> %s\n" (show list1) (show 0) (show $ combinations list1 0)

data NestedList a = Element a | List [NestedList a] deriving Show

data Encoded a = Once a | Replicated (Int, a)

instance (Show a) => Show (Encoded a) where
  show (Once element) = show element
  show (Replicated (replications, element)) = "(" ++ show replications ++ ", " ++ show element ++ ")"

last_ :: [a] -> Maybe a
last_ list = case list of
    [] -> Nothing
    [last] -> Just last
    (_ : tail) -> last_ tail

lastButOne :: [a] -> Maybe a
lastButOne list = case list of
    [] -> Nothing
    [_] -> Nothing
    [lasButOne, _] -> Just lasButOne
    (_ : tail) -> lastButOne tail

elementAt :: [a] -> Int -> Maybe a
elementAt list position = case list of
    [] -> Nothing
    (head : tail) 
        | position < 1 -> Nothing
        | position == 1 -> Just head
        | otherwise -> elementAt tail (position - 1)

length_ :: [a] -> Int
length_ list = length_' list 0
    where length_' :: [a] -> Int -> Int
          length_' list length = case list of
              [] -> length
              (_ : tail) -> length_' tail (length + 1)

reverse_ :: [a] -> [a]
reverse_ list = reverse_' list []
    where reverse_' :: [a] -> [a] -> [a]
          reverse_' list reversed = case list of
              [] -> reversed
              (head : tail) -> reverse_' tail (head : reversed)

isPalindrom :: Eq a => [a] -> Bool
isPalindrom list = list == reverse_ list

flatten :: NestedList a -> [a]
flatten nested = case nested of
    Element element -> [element]
    (List []) -> []
    (List (head : tail)) -> flatten head ++ flatten (List tail)

compress :: Eq a => [a] -> [a]
compress list = compress' list []
    where compress' :: Eq a => [a] -> [a] -> [a]
          compress' list compressed = case list of
              [] -> reverse_ compressed
              [head] -> reverse_ (head : compressed)
              (head : next : tail)
                  | head == next -> compress' (next : tail) compressed
                  | otherwise -> compress' (next : tail) (head : compressed)

pack :: Eq a => [a] -> [[a]]
pack list = pack' list [] []
    where pack' :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
          pack' list subList packed = case list of
              [] -> case subList of
                  [] -> packed
                  _ -> packed ++ [subList]
              (head : tail) -> case subList of
                  [] -> pack' tail [head] packed
                  (current : _)
                      | head == current -> pack' tail (current : subList) packed
                      | otherwise -> pack' tail [head] (packed ++ [subList])

encode :: Eq a => [a] -> [(Int, a)]
encode list = encode' (pack list) []
    where encode' :: Eq a => [[a]] -> [(Int, a)] -> [(Int, a)]
          encode' packed encoded = case packed of
              [] -> reverse_ encoded
              (subList@(head : _) : tail) -> encode' tail ((length_ subList, head) : encoded) 

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified list = encodeModified' (pack list) []
    where encodeModified' :: Eq a => [[a]] -> [Encoded a] -> [Encoded a]
          encodeModified' packed encoded = case packed of
              [] -> reverse_ encoded
              (subList@(head : _) : tail)
                  | length_ subList > 1 -> encodeModified' tail (Replicated (length_ subList, head) : encoded)
                  | otherwise ->encodeModified' tail (Once head : encoded)

replicateHead :: Int -> a -> [a] -> [a]
replicateHead replications head list
    | replications <= 0 = list
    | otherwise = replicateHead (replications - 1) head (head : list)

decodeModified :: [Encoded a] -> [a]
decodeModified encoded = decodeModified' encoded []
    where decodeModified' :: [Encoded a] -> [a] -> [a]
          decodeModified' encoded decoded = case encoded of
              [] -> reverse_ decoded
              (Once head : tail) -> decodeModified' tail (head : decoded)
              (Replicated (replications, head) : tail) -> decodeModified' tail $ replicateHead replications head decoded

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect list = encodeDirect' list []
    where encodeDirect' :: Eq a => [a] -> [Encoded a] -> [Encoded a]
          encodeDirect' list encoded = case list of
            [] -> reverse_ encoded
            (head : tail)
                | replications > 1 -> encodeDirect' reduced (Replicated (replications, head) : encoded)
                | otherwise -> encodeDirect' tail (Once head : encoded)
                where (replications, reduced) = removeReplications head tail 1 
                      removeReplications :: Eq a => a -> [a] -> Int -> (Int, [a])
                      removeReplications current list replications = case list of
                          [] -> (replications, list)
                          (head : tail)
                              | current == head -> removeReplications current tail (replications + 1) 
                              | otherwise -> (replications, list)

dupli :: [a] -> [a]
dupli list = dupli' list []
    where dupli' :: [a] -> [a] -> [a]
          dupli' list duplicated = case list of
              [] -> reverse duplicated
              (head : tail) -> dupli' tail (head : head : duplicated)

repli :: [a] -> Int -> [a]
repli list replications = repli' list replications []
    where repli' :: [a] -> Int -> [a] -> [a]
          repli' list replications replicated = case list of
              [] -> reverse_ replicated
              (head : tail) -> repli' tail replications $ replicateHead replications head replicated

drop_ :: [a] -> Int -> [a]
drop_ list frequency = drop_' list frequency 1 []
        where drop_' :: [a] -> Int -> Int -> [a] -> [a]
              drop_' list frequency count reduced = case list of
                  [] -> reverse_ reduced
                  (head : tail)
                      | count == frequency -> drop_' tail frequency 1 reduced
                      | otherwise -> drop_' tail frequency (count + 1) (head : reduced)

split :: [a] -> Int -> ([a], [a])
split list position = split' list position 1 ([], [])
    where split' :: [a] -> Int -> Int -> ([a], [a]) -> ([a], [a])
          split' list position index (left, right) = case list of
              [] -> (reverse_ left, reverse_ right)
              (head : tail)
                  | index <= position -> split' tail position (index + 1) (head : left, right)
                  | otherwise -> split' tail position (index + 1) (left, head : right)

slice :: [a] -> Int -> Int -> [a]
slice list start end = slice' list start end 1 []
    where slice' :: [a] -> Int -> Int -> Int -> [a] -> [a]
          slice' list start end index slice = case list of
              [] -> reverse_ slice
              (head : tail)
                  | index > end -> reverse_ slice
                  | index >= start && index <= end -> slice' tail start end (index + 1) (head : slice)
                  | otherwise -> slice' tail start end (index + 1) slice

rotate :: Eq a => [a] -> Int -> [a]
rotate list positions
    | positions == 0 || null list = list
    | positions > 0 = snd (split list positions) ++ fst (split list positions)
    | otherwise = snd (split list (length_ list + positions)) ++ fst (split list (length_ list + positions))

removeAt :: [a] -> Int -> ([a], Maybe a)
removeAt list position = removeAt' list position 1 [] Nothing
    where removeAt' :: [a] -> Int -> Int -> [a] -> Maybe a -> ([a], Maybe a)
          removeAt' list position index reduced removed = case list of
              [] -> (reverse_ reduced, removed)
              (head : tail)
                  | index == position -> removeAt' tail position (index + 1) reduced (Just head)
                  | otherwise -> removeAt' tail position (index + 1) (head : reduced) removed
  
insertAt :: [a] -> a -> Int -> [a]
insertAt list value position = insertAt' list value position 1 []
    where insertAt' :: [a] -> a -> Int -> Int -> [a] -> [a]  
          insertAt' list value position index increased = case list of
              [] -> reverse_ increased
              (head : tail)
                  | index == position -> insertAt' tail value position (index + 1) (head : value : increased)
                  | otherwise -> insertAt' tail value position (index + 1) (head : increased)

range :: Int -> Int -> [Int]
range minimum maximum = range' minimum maximum []
    where range' :: Int -> Int -> [Int] -> [Int]
          range' minimum maximum range
              | minimum <= maximum = range' (minimum + 1) maximum (minimum : range)
              | otherwise = reverse_ range

randomSelection :: [a] -> Int -> IO [a]
randomSelection list draws = randomSelection' list draws $ length_ list
    where randomSelection' :: [a] -> Int -> Int -> IO [a]
          randomSelection' list draws length
              | draws <= 0 || null list = return []
              | otherwise = do
                  randomNumber <- Random.randomRIO (1, length)
                  let randomElement = Maybe.fromJust $ snd $ removeAt list randomNumber
                  selection <- randomSelection' (fst $ removeAt list randomNumber) (draws - 1) (length - 1)
                  return (randomElement : selection)

lotto :: Int -> Int -> IO [Int]
lotto draws maximum
    | maximum < draws || maximum <= 0 = return []
    | otherwise = randomSelection (range 1 maximum) draws

randomPermutation :: [a] -> IO [a]
randomPermutation list = randomSelection list $ length_ list

combinations :: [a] -> Int -> [[a]]
combinations list draws = case list of
    [] -> [[]]
    (head : tail)
        | draws <= 0 -> [[]]
        | otherwise -> withHead ++ withoutHead
            where withHead = [ head : combination | combination <- combinations tail (draws - 1) ]
                  withoutHead
                      | draws <= length_ tail = combinations tail draws 
                      | otherwise = []
