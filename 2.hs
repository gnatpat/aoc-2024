import Parser ( Parser, integer, newline, runParser' )
import Control.Applicative (Alternative (..), liftA2)

parse :: Parser [[Int]]
parse = some (some integer <* newline)

mainA :: IO ()
mainA = print . length . filter isSafe . runParser' parse =<< getContents

mainB :: IO ()
mainB = print . length . filter (any isSafe) . map options . runParser' parse =<< getContents
  where options l = l : map (`removeAt` l) [0..(length l-1)]
        removeAt n = take n <> drop (n+1)

main = mainB

isSafe :: [Int] -> Bool
isSafe = gradual . deltas
  where deltas = zipWith (-) <*> tail
        gradual  = (||) <$> all (`elem` [-1,-2,-3]) <*> all (`elem` [1,2,3])