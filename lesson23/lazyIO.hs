{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main :: IO ()
main = do
  uinput <- TIO.getContents
  let numbers = toInts uinput
  TIO.putStrLn ((T.pack . show . sum) numbers)

