import Parser ( Parser, integer, newline, runParser' )
import Control.Applicative (Alternative (..))
import Data.List ( group, sort )
import qualified Data.IntMap as Map

parseInput :: Parser ([Int], [Int])
parseInput = unzip <$> some ((,) <$> integer <*> integer <* newline)

mainA :: IO ()
mainA = do
    print . sum . map abs . uncurry (zipWith (-)) . sortBoth . runParser' parseInput =<< getContents

mainB :: IO ()
mainB = do
    (nums, rawCount) <- runParser' parseInput <$> getContents
    let counts = Map.fromList . map (\g -> (head g, length g)) . group $ sort rawCount
    print . sum $ map (\i -> i * Map.findWithDefault 0 i counts) nums

main = mainB

sortBoth :: ([Int], [Int]) -> ([Int], [Int])
sortBoth (a, b) = (sort a, sort b)