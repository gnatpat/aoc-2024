import Parser ( Parser, integer, newline, runParser', string, char, satisfy)
import Control.Applicative (Alternative (..))
import Data.Maybe (catMaybes)
import Data.Foldable (Foldable(foldl'))

data Instruction = Mul Int Int | Do | Dont
type Input = [Instruction]

parser :: Parser Input
parser = catMaybes <$> some (parseMul <|> parseDont <|> parseDo <|> anyChar)
  where parseMul = Just <$> (Mul <$> (string "mul(" *> integer <* char ',') <*> (integer <* string ")"))
        parseDo = Just <$> (Do <$ string "do()")
        parseDont = Just <$> (Dont <$ string "don't()")
        anyChar = Nothing <$ satisfy (const True)

part1 :: Input -> Int
part1 = sum . map onlyMul
    where onlyMul (Mul a b) = a * b
          onlyMul _ = 0

part2 :: Input -> Int
part2 = snd . foldl' applyInstruction (True, 0)
  where applyInstruction (_, total) Do = (True ,total)
        applyInstruction (_, total) Dont = (False ,total)
        applyInstruction (True, total) (Mul x y) = (True, total + x * y)
        applyInstruction (False, total) (Mul x y) = (False, total)

main :: IO ()
main = print . ((,) <$> part1 <*> part2) . runParser' parser =<<getContents