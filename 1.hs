import Parser
import Control.Applicative (Alternative (..))
import Data.Either (fromRight)
import Data.List
import qualified Data.IntMap as Map

parseInput :: Parser [(Int, Int)]
parseInput = do
    fst <- integer
    whitespace
    snd <- integer
    newline
    rest <- ([] <$ eof) <|> parseInput
    return ((fst, snd):rest)

mainA :: IO ()
mainA = do
    print . sum . map abs . uncurry (zipWith (-)) . sortBoth . unzip . runParser' parseInput =<< getContents

mainB :: IO ()
mainB = do
    (nums, rawCount) <- unzip . runParser' parseInput <$> getContents
    let counts = Map.fromList . map (\g -> (head g, length g)) . group $ sort rawCount
    print . sum $ map (\i -> i * Map.findWithDefault 0 i counts) nums

main = mainB

sortBoth :: ([Int], [Int]) -> ([Int], [Int])
sortBoth (a, b) = (sort a, sort b)