import qualified Text.Printf as Text (printf)

-- encoding : a -> 1, b -> 2, ... , z -> 26
main :: IO ()
main = do
    let count1 = countDigitSequenceDecodings "111" -- aaa, ak, ka
    Text.printf "%s -> %s\n" "111" (show count1)

    let count2 = countDigitSequenceDecodings "1234" -- abcd, lcd, awd
    Text.printf "%s -> %s\n" "1234" (show count2)

    let count3 = countDigitSequenceDecodings "102" -- jb
    Text.printf "%s -> %s\n" "102" (show count3)

countDigitSequenceDecodings :: String -> Int
countDigitSequenceDecodings code = case code of
    [] -> 1
    [_] -> 1
    (firstDigit : secondDigit : rest) -> oneElementalDecoding + twoElementalDecoding
        where oneElementalDecoding
                | firstDigit > '0' = countDigitSequenceDecodings (secondDigit : rest)
                | otherwise = 0
              twoElementalDecoding
                | firstDigit == '1' ||
                  firstDigit == '2' && secondDigit < '7' = countDigitSequenceDecodings rest
                | otherwise = 0
