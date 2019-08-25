import qualified Text.Printf as Text (printf)
import qualified Data.Maybe as Maybe (fromJust)

main :: IO ()
main = do
    Text.printf "%s -> %s\n" "([])[]({})" (show $ isBracketingValid "([])[]({})")

    Text.printf "%s -> %s\n" "([)]" (show $ isBracketingValid "([)]")

    Text.printf "%s -> %s\n" "((()" (show $ isBracketingValid "((()")

    Text.printf "%s -> %s\n" "" (show $ isBracketingValid "")

data Stack a = Empty | Top a (Stack a) deriving (Show, Eq)

push :: a -> Stack a -> Stack a
push element stack = case stack of
    Empty -> Top element Empty
    otherwise -> Top element stack

pop :: Stack a -> (Maybe a, Stack a)
pop stack = case stack of
    Empty -> (Nothing, Empty)
    (Top element rest) -> (Just element, rest)

isBracketingValid :: String -> Bool
isBracketingValid bracketString = isBracketingValid' bracketString Empty
    where isBracketingValid' :: String -> Stack Char -> Bool
          isBracketingValid' bracketString stack = case bracketString of
            [] -> stack == Empty
            (currentBracket : rest)
                | currentBracket == '(' ||
                  currentBracket == '[' ||
                  currentBracket == '{' -> isBracketingValid' rest $ push currentBracket stack
                | currentBracket == ')' -> previousBracket == '(' && isRestValid
                | currentBracket == ']' -> previousBracket == '[' && isRestValid
                | currentBracket == '}' -> previousBracket == '{' && isRestValid
                where previousBracket = Maybe.fromJust $ fst $ pop stack
                      isRestValid = isBracketingValid' rest $ snd $ pop stack
