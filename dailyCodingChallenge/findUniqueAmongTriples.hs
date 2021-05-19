import qualified Text.Printf as Text (printf)
import qualified Data.Int as Int (Int32)
import qualified Data.Bits as Bits ((.|.), shiftL, (.&.))

main :: IO ()
main = do
    Text.printf "%s -> %s\n" (show [6, 1, 3, 3, 3, 6, 6]) (show $ findUniqueAmongTriples [6, 1, 3, 3, 3, 6, 6])

    Text.printf "%s -> %s\n" (show [13, 19, 13, 13]) (show $ findUniqueAmongTriples [13, 19, 13, 13])

    Text.printf "%s -> %s\n" (show [10, -13, -19, -13, -13, 10, 10])
        (show $ findUniqueAmongTriples [10, -13, -19, -13, -13, 10, 10])

findUniqueAmongTriples :: [Int.Int32] -> Int.Int32
findUniqueAmongTriples list = findUniqueAmongTriples' list 0 0
    where findUniqueAmongTriples' :: [Int.Int32] -> Int -> Int.Int32 -> Int.Int32
          findUniqueAmongTriples' _ 31 uniqueElement
            | uniqueElement Bits..&. 1 `Bits.shiftL` 30 > 0 = uniqueElement + 2 ^ 31
            | otherwise = uniqueElement
          findUniqueAmongTriples' list bitPosition uniqueElement =
              if countOnesAtBitPosition list bitPosition 0 `mod` 3 /= 0
              then findUniqueAmongTriples' list (bitPosition + 1) (uniqueElement Bits..|. oneAtBitPosition)
              else findUniqueAmongTriples' list (bitPosition + 1) uniqueElement
              where oneAtBitPosition = 1 `Bits.shiftL` bitPosition
                    countOnesAtBitPosition :: [Int.Int32] -> Int -> Int.Int32 -> Int.Int32
                    countOnesAtBitPosition [] _ onesCount = onesCount
                    countOnesAtBitPosition (element : rest) bitPosition onesCount =
                        if element Bits..&. oneAtBitPosition > 0
                        then countOnesAtBitPosition rest bitPosition (onesCount + 1)
                        else countOnesAtBitPosition rest bitPosition onesCount
