import Parser ( Parser, integer, newline, runParser' )
import Control.Applicative (Alternative (..))

type Input = ()

parser :: Parser Input
parser = return ()

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

main :: IO ()
main = print . ((,) <$> part1 <*> part2) . runParser' parser =<<getContents