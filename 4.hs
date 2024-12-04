import Parser ( Parser, integer, newline, runParser', anyChar, oneOf )
import Control.Applicative (Alternative (..))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (isPrefixOf)

type Input = Map (Int, Int) Char

parser :: Parser Input
parser = makeMap <$> some (some (oneOf "XMAS") <* newline)

makeMap :: [[Char]] -> Map (Int, Int) Char
makeMap = Map.fromList . concat . zipWith (\y -> map (\(x, c) -> ((x, y), c))) [0..] . map (zip [0..])

part1 :: Input -> Int
part1 = length . findOccurances "XMAS"

findOccurances word grid = filter (isPrefixOf word . snd) $ allPaths grid
allPaths grid = (zip <*> map (uncurry (pathInDir grid))) ((,) <$> directions <*> Map.keys grid)
directions  = [(x, y) | x<-[-1..1], y<-[-1..1], x/=0 || y/=0]
pathInDir  grid d p | p `Map.member` grid = (grid ! p) : pathInDir grid d (p `add` d)
                    | otherwise = []
add (x1, y1) (x2, y2) = (x1+x2, y1+y2)


part2 :: Input -> Int
part2 = (`div` 2) . length . filter (uncurry isCrossing) . combos2 . findOccurances "MAS"
    where combos2 l = [(a, b) | a <- l, b <- l]
          isCrossing ((d1, p1), _) ((d2, p2), _) = c1 == c2 && areOrthogonal d1 d2 && isDiagonal d1
                where c1 = d1 `add` p1
                      c2 = d2 `add` p2
          isDiagonal (dx, dy) = dx /= 0 && dy /= 0
          areOrthogonal d1 d2 = d1 == rotate90 d2 || d1 == rotate270 d2
          rotate90 (dx, dy) = (-dy, dx)
          rotate270 = rotate90 . rotate90 . rotate90

main :: IO ()
main = print . ((,) <$> part1 <*> part2) . runParser' parser =<<getContents