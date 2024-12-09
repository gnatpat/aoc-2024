import Parser ( Parser, integer, newline, runParser', char )
import Control.Applicative (Alternative (..))
import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortBy)

type Page = Int
type Rule = (Page, Page)
type Update = [Page]
type Input = (Set Rule, [Update])

parser :: Parser Input
parser = do
    rules <- Set.fromList <$> some ((,) <$> (integer <* char '|') <*> (integer <* newline) )
    newline
    updates <- some ((:) <$> integer <*> (some (char ',' *> integer) <* newline))
    return (rules, updates)

part1 :: Input -> Int
part1 (rules, updates) = sum . map middleNum $ filter (correctlyPrinted rules) updates

correctlyPrinted :: Ord b => Set (b, b) -> [b] -> Bool
correctlyPrinted rules update = not . any ((`Set.member` rules) . swap) $ orderedPairCombos update
    where orderedPairCombos [x] = []
          orderedPairCombos (x:xs) = map (x,) xs ++ orderedPairCombos xs

middleNum :: [a] -> a
middleNum l = l !! (length l `div` 2)

part2 :: Input -> Int
part2 (rules, updates) = sum . map (middleNum . sortBy (compareRules rules)) $ filter (not . correctlyPrinted rules) updates
    where compareRules rules a b | (a, b) `Set.member` rules = LT
                                 | (b, a) `Set.member` rules = GT
                                 | otherwise = EQ

main :: IO ()
main = print . ((,) <$> part1 <*> part2) . runParser' parser =<<getContents