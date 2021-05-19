import qualified Text.Printf as Text (printf)
import qualified Data.List as List (tails)
import qualified Data.Maybe as Maybe (fromJust)

main :: IO ()
main = do
    Text.printf "%s, %s -> %s\n" (show 17) (show [15, 7, 3, 10]) (show $ Maybe.fromJust $ pairSumsToNumber 17 [15, 7, 3, 10])

    Text.printf "%s, %s -> %s\n" (show 2) (show [0, 1]) (show $ pairSumsToNumber 2 [0, 1])

    Text.printf "%s, %s -> %s\n" (show 0) (show [-1, 0, 1]) (show $ Maybe.fromJust $ pairSumsToNumber 0 [-1, 0, 1])

    Text.printf "%s, %s -> %s\n" (show 10) (show [5, 0, 5]) (show $ Maybe.fromJust $ pairSumsToNumber 10 [5, 0, 5])

pairSumsToNumber :: Int -> [Int] -> Maybe(Int, Int)
pairSumsToNumber number list = case pairs of
    [] -> Nothing
    (pair : _) -> Just pair
    where pairs = [(a, b) | (a : tailWithoutA) <- List.tails list, b <- tailWithoutA, a + b == number]
