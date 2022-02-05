import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

-- Q24 . 1 Write a version of the Unix cp program that will copy a file and
-- allow you to rename it (just mimic the basic functionality and don’t worry
-- about specific flags).

main :: IO ()
main = do
  args <- getArgs
  let srcFile = head args -- args !! 0
  let destFile = args !! 1
  contents <- TIO.readFile srcFile
  TIO.writeFile destFile contents
