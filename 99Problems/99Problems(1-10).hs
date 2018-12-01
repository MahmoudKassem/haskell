import qualified Text.Printf as Text (printf)
import qualified Data.Maybe as Maybe (fromJust)

main :: IO ()
main = do
    let list1 = [1, 2, 3, 4]
    let list2 = ['a', 'b', 'c', 'd']
    let list3 = tail [1]
    let list4 = [1]
    let list5 = [1, 3, 1]
    let list6 = ['a', 'b', 'c', 'c', 'b', 'a']
    let list7 = (List [Element 1, Element 2, (List [Element 3, (List [Element 4, Element 5])]), Element 6])
    let list8 = (List [Element 'a', Element 'b', (List [Element 'c', (List [Element 'd', Element 'e'])]), Element 'f'])
    let list9 = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    let list10 = [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5]

    putStrLn "#1 last element of a list"
    Text.printf "%s -> %s\n" (show list1) (show $ Maybe.fromJust $ myLast list1)
    Text.printf "%s -> %s\n" (show list2) (show $ Maybe.fromJust $ myLast list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ myLast list3)

    putStrLn "#2 last but one element of a list"
    Text.printf "%s -> %s\n" (show list1) (show $ Maybe.fromJust $ lastButOne list1)
    Text.printf "%s -> %s\n" (show list2) (show $ Maybe.fromJust $ lastButOne list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ lastButOne list3)

    putStrLn "#3 k'th element of a list"
    Text.printf "%s, %s -> %s\n" (show list1) (show 2) (show $ Maybe.fromJust $ elementAt list1 2)
    Text.printf "%s, %s -> %s\n" (show list2) (show 2) (show $ Maybe.fromJust $ elementAt list2 2)
    Text.printf "%s, %s -> %s\n\n" (show list3) (show 2) (show $ elementAt list3 2)

    putStrLn "#4 number of elements in a list"
    Text.printf "%s -> %s\n" (show list1) (show $ myLength list1)
    Text.printf "%s -> %s\n" (show list2) (show $ myLength list2)
    Text.printf "%s -> %s\n\n" (show list3) (show $ myLength list3)

    putStrLn "#5 reverse a list"
    Text.printf "%s -> %s\n" (show list1) (show  $ myReverse list1)
    Text.printf "%s -> %s\n" (show list2) (show  $ myReverse list2)
    Text.printf "%s -> %s\n\n" (show list3) (show  $ myReverse list3)

    putStrLn "#6 is list a palindrome"
    Text.printf "%s -> %s\n" (show list1) (show $ isPalindrom list1)
    Text.printf "%s -> %s\n" (show list2) (show $ isPalindrom list2)
    Text.printf "%s -> %s\n" (show list3) (show $ isPalindrom list3)
    Text.printf "%s -> %s\n" (show list4) (show $ isPalindrom list4)
    Text.printf "%s -> %s\n" (show list5) (show $ isPalindrom list5)
    Text.printf "%s -> %s\n\n" (show list6) (show $ isPalindrom list6)

    putStrLn "#7 flatten a nested list"
    Text.printf "%s -> %s\n" (show list7) (show $ myFlatten list7)
    Text.printf "%s -> %s\n\n" (show list8) (show $ myFlatten list8)

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

myLast :: [a] -> Maybe a
myLast list = case list of
    [] -> Nothing
    [lastElement] -> Just lastElement
    (_ : rest) -> myLast rest

lastButOne :: [a] -> Maybe a
lastButOne list = case list of
    [] -> Nothing
    [_] -> Nothing
    [lasButOneElement, _] -> Just lasButOneElement
    (_ : rest) -> lastButOne rest

elementAt :: [a] -> Int -> Maybe a
elementAt list position =
    if position < 1 then Nothing
    else if position == 1 then case list of
        [] -> Nothing
        (element : _) -> Just element
    else case list of
        [] -> Nothing
        (_ : rest) -> elementAt rest (position - 1)

myLength :: [a] -> Int
myLength list = myLength' list 0
    where myLength' list listLength = case list of
            [] -> listLength
            (_ : rest) -> myLength' rest (1 + listLength)

myReverse :: [a] -> [a]
myReverse list = myReverse' list []
    where myReverse' list reversedList = case list of
            [] -> reversedList
            (element : rest) -> myReverse' rest (element : reversedList)

isPalindrom :: Eq a => [a] -> Bool
isPalindrom list = list == myReverse list

data NestedList a = Element a | List [NestedList a] deriving Show

myFlatten :: NestedList a -> [a]
myFlatten nestedList = case nestedList of
    Element element -> [element]
    (List []) -> []
    (List (element : rest)) -> myFlatten element ++ myFlatten (List rest)

compress :: Eq a => [a] -> [a]
compress list = compress' list []
    where compress' list compressedList = case list of
            [] -> compressedList
            [element] -> (compressedList ++ [element])
            (element : nextElement : rest)
                | element == nextElement -> compress' (nextElement : rest) compressedList
                | otherwise -> compress' (nextElement : rest) (compressedList ++ [element])

pack :: Eq a => [a] -> [[a]]
pack list = pack' list [] []
    where pack' list consecutiveDuplicatesList packedList = case list of
            [] -> case consecutiveDuplicatesList of
                [] -> packedList
                otherwise -> (packedList ++ [consecutiveDuplicatesList])
            (element : rest) -> case consecutiveDuplicatesList of
                [] -> pack' rest (element : consecutiveDuplicatesList) packedList
                (duplicateElement : _)
                    | element == duplicateElement -> pack' rest (element : consecutiveDuplicatesList) packedList
                    | otherwise -> pack' rest [element] (packedList ++ [consecutiveDuplicatesList])

encode :: Eq a => [a] -> [(Int, a)]
encode list = encode' (pack list) []
    where encode' packedList encodedList = case packedList of
            [] -> encodedList
            (packedSublist : packedRest) -> case packedSublist of
                (subListElement : _) -> encode' packedRest (encodedList ++ [(myLength packedSublist, subListElement)])
