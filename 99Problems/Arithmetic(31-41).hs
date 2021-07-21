import qualified Text.Printf as Text (printf)
import qualified Data.Time.Clock as Time (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
    putStrLn "#31 determine whether a given integer number is prime"
    Text.printf "%s -> %s\n" (show 1) (show $ isPrime 1)
    Text.printf "%s -> %s\n\n" (show 31) (show $ isPrime 31)

    putStrLn "#32 determine the prime factors of a given positive integer"
    Text.printf "%s -> %s\n" (show 1) (show $ primeFactors 1)
    Text.printf "%s -> %s\n\n" (show 315) (show $ primeFactors 315)

    putStrLn "#33 determine the prime factors of a given positive integer (2)"
    Text.printf "%s -> %s\n" (show 1) (show $ primeFactorsMult 1)
    Text.printf "%s -> %s\n\n" (show 315) (show $ primeFactorsMult 315)

    putStrLn "#34 a list of prime numbers"
    Text.printf "%s, %s -> %s\n" (show 1) (show 2) (show $ primesList 1 2)
    Text.printf "%s, %s -> %s\n\n" (show 3) (show 15) (show $ primesList 3 15)

    putStrLn "#35 goldbach's conjecture"
    Text.printf "%s -> %s\n" (show 28) (show $ goldbach 28)
    Text.printf "%s -> %s\n\n" (show 256) (show $ goldbach 256)

    putStrLn "#36 a list of Goldbach compositions"
    Text.printf "%s, %s -> %s\n" (show 1) (show 4) (show $ goldbachList 1 4)
    Text.printf "%s, %s -> %s\n\n" (show 4) (show 16) (show $ goldbachList 4 16)

    putStrLn "#37 determine the greatest common divisor of two positive integer numbers"
    Text.printf "%s, %s -> %s\n" (show 7) (show 23) (show $ greatestCommonDivisor 7 23)
    Text.printf "%s, %s -> %s\n\n" (show 36) (show 63) (show $ greatestCommonDivisor 36 63)

    putStrLn "#38 determine whether two positive integer numbers are coprime"
    Text.printf "%s, %s -> %s\n" (show 36) (show 63) (show $ coprime 36 63)
    Text.printf "%s, %s -> %s\n\n" (show 7) (show 23) (show $ coprime 7 23)

    putStrLn "#39 calculate Euler's totient function phi(m)"
    Text.printf "%s -> %s\n" (show 1) (show $ totient 1)
    Text.printf "%s -> %s\n\n" (show 10) (show $ totient 10)

    putStrLn "#40 calculate Euler's totient function phi(m) (2)"
    Text.printf "%s -> %s\n" (show 1) (show $ totientEfficient 1)
    Text.printf "%s -> %s\n\n" (show 10) (show $ totientEfficient 10)

    putStrLn "#41 compare the two methods of calculating Euler's totient function"
    start <- Time.getCurrentTime
    let initialImplementation = totient 10090
    end <- Time.getCurrentTime
    Text.printf "intial implementation with %s -> %s took %s\n" (show 10090) (show initialImplementation) (show $ Time.diffUTCTime end start)

    start <- Time.getCurrentTime
    let improvedImplementation = totientEfficient 10090
    end <- Time.getCurrentTime
    Text.printf "improved implementation with %s -> %s took %s\n" (show 10090) (show improvedImplementation) (show $ Time.diffUTCTime end start)

isPrime :: Int -> Bool
isPrime number = number >= 2 && isPrime' 2
    where isPrime' :: Int -> Bool
          isPrime' i
              | i * i > number = True
              | number `mod` i == 0 = False
              | otherwise = isPrime' (i + 1)

primeFactors :: Int -> [Int]
primeFactors number = reverse $ primeFactors' number 3 []
    where primeFactors' :: Int -> Int -> [Int] -> [Int]
          primeFactors' number i primeFactors
              | even number = primeFactors' (number `div` 2) i (2 : primeFactors)
              | number `mod` i == 0 = primeFactors' (number `div` i) i (i : primeFactors)
              | i * i <= number = primeFactors' number (i + 2) primeFactors
              | number > 2 = number : primeFactors
              | otherwise = primeFactors

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult number = primeFactorsMult' (primeFactors number) []
    where primeFactorsMult' :: [Int] -> [(Int, Int)] -> [(Int, Int)]
          primeFactorsMult' primeFactors primeFactorsMult = case primeFactors of
              [] -> reverse primeFactorsMult
              (head : tail)
                  | multiplicity > 1 -> primeFactorsMult' reduced ((head, multiplicity) : primeFactorsMult)
                  | otherwise -> primeFactorsMult' tail ((head, 1) : primeFactorsMult)
                      where (multiplicity, reduced) = removeMultiples head tail 1
                            removeMultiples :: Int -> [Int] -> Int -> (Int, [Int])
                            removeMultiples current list multiplicity = case list of
                                [] -> (multiplicity, list)
                                (head : tail)
                                    | current == head -> removeMultiples current tail (multiplicity + 1)
                                    | otherwise -> (multiplicity, list)

primesList :: Int -> Int -> [Int]
primesList start end
    | start < 2 && end < 2 || start > end = []
    | otherwise = [prime | prime <- [start .. end], isPrime prime]

goldbach :: Int -> (Int, Int)
goldbach number
    | number < 4 || odd number = (0, 0)
    | otherwise = head [(a, b) | a <- primesList 2 number, b <- primesList 2 number, a + b == number]

goldbachList :: Int -> Int -> [(Int, Int, Int)]
goldbachList start end
    | start < 4 && end < 4 || start > end = []
    | end == 4 = [(4, 2, 2)]
    | otherwise = [(number, fst $ goldbach number, snd $ goldbach number) | number <- [start, start + 2 .. end]]

greatestCommonDivisor :: Int -> Int -> Int
greatestCommonDivisor a b
    | b == 0 = a
    | otherwise = greatestCommonDivisor b (a `mod` b)

coprime :: Int -> Int -> Bool
coprime a b = greatestCommonDivisor a b == 1

totient :: Int -> Int
totient number
    | number == 1 = 1
    | otherwise = totient' 1 0
        where totient' :: Int -> Int -> Int
              totient' i totient
                  | i >= number = totient
                  | coprime number i = totient' (i + 1) (totient + 1)
                  | otherwise = totient' (i + 1) totient

totientEfficient :: Int -> Int
totientEfficient number = product [(primeFactor - 1) * primeFactor ^ (multiplicity - 1) | (primeFactor, multiplicity) <- primeFactorsMult number]
