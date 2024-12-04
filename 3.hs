import Parser ( Parser, integer, newline, runParser', string, char, satisfy)
import Control.Applicative (Alternative (..), Applicative (liftA2))
import Data.Maybe (catMaybes)
import Data.Foldable (Foldable(foldl'))

data Instruction = Mul Int Int | Do | Dont
type Input = [Instruction]

parser :: Parser Input
parser = catMaybes <$> some (parseMul <|> parseDont <|> parseDo <|> anyChar)
  where parseMul = Just <$> liftA2 Mul (string "mul(" *> integer <* char ',') (integer <* string ")")
        parseDont = Just Dont <$ string "don't()"
        parseDo = Just Do <$ string "do()"
        anyChar = Nothing <$ satisfy (const True)

part1 :: Input -> Int
part1 = sum . map onlyMul
    where onlyMul (Mul a b) = a * b
          onlyMul _ = 0

part2 :: Input -> Int
part2 = snd . foldl' applyInstruction (True, 0)
  where applyInstruction (_, total) Do = (True, total)
        applyInstruction (_, total) Dont = (False, total)
        applyInstruction (enabled, total) (Mul x y) = (enabled, total + (if enabled then x * y else 0))

main :: IO ()
main = print . ((,) <$> part1 <*> part2) . runParser' parser =<<getContents