import qualified Text.Printf as Text (printf)

main :: IO ()
main = do
    Text.printf "%s, %s -> %s\n" "kitten" "sitting" (show $ editDistance "kitten" "sitting")

    Text.printf "%s, %s -> %s\n" "test" "tset" (show $ editDistance "test" "tset")

    Text.printf "%s, %s -> %s\n" "giraf" "farig" (show $ editDistance "giraf" "farig")

editDistance :: String -> String -> Int
editDistance string1 string2 = editDistance' string1 string2 0
    where editDistance' :: String -> String -> Int -> Int
          editDistance' string1 string2 distance
            | string1 == "" = distance + length string2
            | string2 == "" = distance + length string1
            | otherwise =
                if (head string1) /= (head string2)
                then editDistance' (tail string1) (tail string2) (distance + 1)
                else editDistance' (tail string1) (tail string2) distance
