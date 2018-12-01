import qualified Text.Printf as Text (printf)

main :: IO ()
main = do
    let distance1 = editDistance "kitten" "sitting"
    Text.printf "%s, %s -> %s\n" "kitten" "sitting" (show distance1)

    let distance2 = editDistance "test" "tset"
    Text.printf "%s, %s -> %s\n" "test" "tset" (show distance2)

    let distance3 = editDistance "giraf" "farig"
    Text.printf "%s, %s -> %s\n" "giraf" "farig" (show distance3)

editDistance :: String -> String -> Int
editDistance string1 string2 = editDistance' string1 string2 0
    where editDistance' string1 string2 distance
            | string1 == string2 = distance
            | string1 == "" = distance + length string2
            | string2 == "" = distance + length string1
            | otherwise = if (head string1) /= (head string2)
                          then editDistance' (tail string1) (tail string2) (distance + 1)
                          else editDistance' (tail string1) (tail string2) distance
