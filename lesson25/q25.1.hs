import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let source = head args
  input <- B.readFile source
  putStrLn "Bytes: "
  print (B.length input)
  putStrLn "Characters: "
  print ((T.length . E.decodeUtf8) input)
