import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

-- Q24.2 Write a program called capitalize.hs that will take a file as an
-- argument, read that file, and then rewrite it capitalized.

main :: IO ()
main = do
  args <- getArgs
  let fName = head args
  contents <- TIO.readFile fName
  TIO.writeFile fName (T.toUpper contents)
