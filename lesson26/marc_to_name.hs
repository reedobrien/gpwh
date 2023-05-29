{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Char as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO

type Author = T.Text

type Html = T.Text

type Title = T.Text

data Book = Book
  { author :: Author,
    title :: Title
  }
  deriving (Show)

bookToHtml :: Book -> Html
bookToHtml book =
  mconcat
    [ "<p>\n",
      titleInTags,
      authorInTags,
      "</p>\n"
    ]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em></br>\n"]

book1 :: Book
book1 =
  Book
    { title = "The Conspiracy Against the Human Race",
      author = "Ligotti, Thomas"
    }

book2 :: Book
book2 =
  Book
    { title = "A Short History of Decay",
      author = "Cioran, Emil"
    }

book3 :: Book
book3 =
  Book
    { title = "The Tears of Eros",
      author = "Bataille, Georges"
    }

books :: [Book]
books = [book1, book2, book3]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<html>\n",
      "<head><title>Books</title>\n",
      "<meta charset='utf-8' />",
      "</head>\n",
      "<body>\n",
      booksHtml,
      "\n</body>",
      "</html>"
    ]
  where
    booksHtml = mconcat . map bookToHtml $ books

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
-- getLeader record = B.take leaderLength record
getLeader = B.take leaderLength

-- Parse the int valued length.
rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

-- Extract the int valued lenght from the leader
getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

-- extract next record in chunck from a MARC file returning (x:xs).
nextAndRest :: B.ByteString -> (MarcLeaderRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where
    recordLength = getRecordLength marcStream

-- Convert the whole bytestream (file) into list of records.
allRecords :: B.ByteString -> [MarcLeaderRaw]
allRecords marcStream =
  if marcStream == B.empty
    then []
    else next : allRecords rest
  where
    (next, rest) = nextAndRest marcStream

-- The directory after the leader with information about the record.
type MarcDirectoryRaw = B.ByteString

-- The leader includes the start of the record and we is a fixed size. So we
-- can use that to find the directory length.

-- Get the base address of the record to determine the start of the record.
getBaseAddr :: MarcLeaderRaw -> Int
getBaseAddr leader = rawToInt (B.take 5 remainder)
  where
    remainder = B.drop 12 leader

-- getDirectoryLength uses getBaseAddr to find the directory lenght.
getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddr leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLegnth afterLeader
  where
    directoryLegnth = getDirectoryLength record
    afterLeader = B.drop leaderLength record

-- Each marc directory contains metadata which is 12 bytes long per entry.
type MarcDirectoryEntryRaw = B.ByteString

directoryEntryLength :: Int
directoryEntryLength = 12

-- splitDirectory splits the directory into individual metadata entries.
splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if directory == B.empty
    then []
    else next : splitDirectory rest
  where
    (next, rest) = B.splitAt directoryEntryLength directory

-- All marc directory entries are shaped like this -- a tag at a location.
data FieldMetadata = FieldMetadata
  { tag :: T.Text,
    fieldLen :: Int,
    fieldStart :: Int
  }
  deriving (Show)

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata {tag = textTag, fieldLen = len, fieldStart = start}
  where
    (theTag, rest) = B.splitAt 3 entry
    textTag = E.decodeUtf8 theTag
    (rawLength, rawStart) = B.splitAt 4 rest
    len = rawToInt rawLength
    start = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 baseStringValue
  where
    recordLen = getRecordLength record
    baseAddr = getBaseAddr record
    baseRecord = B.drop baseAddr record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    baseStringValue = B.take (fieldLen fieldMetadata) baseAtEntry

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if null results
    then Nothing
    else Just (head results)
  where
    metadata = (getFieldMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
  if null results
    then Nothing
    else Just ((take len ((T.drop 1 . head)) -1) results)
  where
    rawField = getTextField record fieldMetadata
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record =
  lookupSubfield
    entryMetadata
    subfield
    record
  where
    entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs =
  map
    ( \(title, author) ->
        Book
          { title = fromJust title,
            author = fromJust author
          }
    )
    justPairs
  where
    justPairs =
      filter
        ( \(title, author) ->
            isJust title
              && isJust author
        )
        pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

main :: IO ()
-- main = TIO.writeFile "books.html" (booksToHtml books)
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed

-- let marcRecords = allRecords marcData
-- print (length marcRecords)
