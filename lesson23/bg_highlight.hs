{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

dharma :: T.Text
dharma = T.pack "धर्"
-- dharma :: T.Text
-- dharma = "धर्म" -- Looks like a space

bgText :: T.Text
bgText = T.pack "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो"
-- bgText :: T.Text
-- bgText = " श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो" -- space in the first char position?

-- Listing 23.10. The highlight function for highlighting text segments
highlight :: T.Text -> T.Text -> T.Text
highlight query txt = T.intercalate highlighted pieces
    where pieces = T.splitOn query txt
          highlighted = mconcat ["{", query, "}"]

main :: IO ()
main = do
    TIO.putStrLn (highlight dharma bgText)
