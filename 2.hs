import Parser ( Parser, integer, newline, runParser' )
import Control.Applicative (Alternative (..))
import Data.Either (fromRight)
import Data.List ( group, sort )
import qualified Data.IntMap as Map

parse :: Parser [[Int]]
parse = some (some integer <* newline)

mainA :: IO ()
mainA = do
    print . length . filter isSafe . map pairDiff . runParser' parse =<< getContents

mainB :: IO ()
mainB = do
    options <- map options . runParser' parse <$> getContents
    print . length . filter (not . null) $ filter isSafe . map pairDiff <$> options

main = mainB

pairDiff :: [Int] -> [Int]
pairDiff l = zipWith (flip (-)) l (tail l)

isSafe :: [Int] -> Bool
isSafe l = all (`elem` [-1,-2,-3]) l || all (`elem` [1,2,3]) l

removeAt :: Int -> [a] -> [a]
removeAt n l = take n l ++ drop (n+1) l

options :: [a] -> [[a]]
options l = l : map (`removeAt` l) [0..(length l-1)]
