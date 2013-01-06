{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Compression.GZip ( compress )
import Control.Exception ( assert )
import Control.Monad ( filterM
                     , forM
                     , forM_
                     , liftM
                     )
import Data.Binary ( Binary
                   , encode )
import qualified Data.ByteString.Base64.Lazy as B ( encode )
import qualified Data.ByteString.Lazy.Char8 as L ( unpack )
import Data.Char ( ord )
import Data.Map ( findWithDefault
                , foldrWithKey
                , fromList
                , Map
                , toList
                )
import qualified Data.Map as M ( lookup )
import Data.Maybe ( fromJust )
import Data.Monoid ( (<>) )
import qualified Data.Text as T ( breakOnAll
                                , chunksOf
                                , intercalate
                                , length
                                , lines
                                , null
                                , pack
                                , singleton
                                , strip
                                , Text
                                , toLower
                                , unpack
                                , words
                                )
import qualified Data.Text.IO as TIO ( putStrLn )
import Network.HTTP ( HeaderName(HdrUserAgent)
                    , RequestMethod(GET)
                    , rspBody
                    , simpleHTTP
                    )
import Network.HTTP.Base ( Request(..) )
import Network.HTTP.Headers ( Header(..) )
import Network.URI ( parseURI )
import System.Directory ( doesFileExist )
import System.Environment ( getArgs )
import System.IO ( hClose
                 , hPutStr
                 , hPutStrLn
                 , IOMode(WriteMode)
                 , openFile
                 , stderr
                 )
import Text.Printf ( printf )
import Text.Regex.PCRE ( (=~) )
import Text.Regex.Base.RegexLike ( AllTextSubmatches(getAllTextSubmatches) )
import Text.XML.HaXml ( render )
import Text.XML.HaXml.Combinators ( deep
                                  , tag
                                  , txt
                                  , (/>)
                                  )
import Text.XML.HaXml.Html.Parse ( htmlParse )
import Text.XML.HaXml.Html.Pretty ( content )
import Text.XML.HaXml.Posn ( posInNewCxt )
import Text.XML.HaXml.Types ()
import Text.XML.HaXml.Util ( docContent )
import Text.XML.HaXml.Wrappers ()

{- | Builds an adjacency graph as a dictionary: {character:
  [adjacent_characters]}.
  Adjacent characters occur in a clockwise order.
  For example:
  - on qwerty layout, 'g' maps to ['fF', 'tT', 'yY', 'hH', 'bB', 'vV']
  - on keypad layout, '7' maps to [None, None, None, '=', '8', '5', '4', None]
-}
buildGraph :: T.Text               -- keyboard layout
           -> Bool                 -- True if the keyboard is slanted
           -> Map T.Text [T.Text]  -- Map of keys adjacent to a given key
buildGraph layout slanted =
  let
    tokens = T.words $ T.strip layout
    tokenSize =
      let
        value = T.length $ head tokens
      in
        assert (all (\token -> value == T.length token) tokens) value
    xUnit = tokenSize + 1  -- X position unit length is token length plus 1
                           -- for the following whitespace.
    adjacencyFunction =
      if slanted
      then getSlantedAdjacentCoords
      else getAlignedAdjacentCoords
    positionTable =
      fromList [ ((x, y), token)
        | (line, y) <- zip (T.lines layout) from1
        , let
            slant = if slanted then y - 1 else 0
        , token <- T.words $ T.strip line
        , let
            pos = fromJust (indexOf token line) - slant
            x = assert (pos `mod` xUnit == 0) $ pos `div` xUnit
        ]
  in
    fromList $ concat $ foldrWithKey (\(x, y) chars acc ->
      map (\char ->
        (T.singleton char,
          map (\coord ->
            findWithDefault "" coord positionTable) $
              adjacencyFunction x y)) (T.unpack chars) : acc)
      [] positionTable

dvorak :: T.Text
dvorak =
  "`~ 1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0) [{ ]}\n\
  \    '\" ,< .> pP yY fF gG cC rR lL /? =+ \\|\n\
  \     aA oO eE uU iI dD hH tT nN sS -_\n\
  \      ;: qQ jJ kK xX bB mM wW vV zZ"

filterAscii :: [T.Text]  -- ^ terms to filter
            -> [T.Text]  -- ^ filtered terms
filterAscii text =
  map T.pack $filter (all (\ chr -> ord chr < 128)) $ map T.unpack text

-- | Remove duplicate terms from a term list if the rank of a term > the rank of
-- the same term in another term list
filterDup :: [T.Text]          -- term list to trim of duplicates
          -> Map T.Text Int    -- ranks of the terms in the list to trim
          -> [Map T.Text Int]  -- other term sets to exclude
          -> [T.Text]          -- return term list with duplicates removed
filterDup terms termsMap maps =
  let
    maxRank = length terms
  in
    filter (\term ->
      all (\theMap ->
        fromJust (M.lookup term termsMap) <
          findWithDefault maxRank term theMap) maps) terms

-- | Compose filterAscii and filterShort.
filtered :: [T.Text]  -- ^ terms to filter
         -> [T.Text]  -- ^ filtered terms
filtered = filterAscii . filterShort

filterShort :: [T.Text]  -- ^ terms to filter
            -> [T.Text]  -- ^ filtered terms
filterShort terms = map fst $ filter long $ zip terms from1
  where long (term, index) = (26 ^ T.length term) > index

-- | Infinite list of integers starting with zero.
from0 :: [Int]
from0 = [0..]

-- | Infinite list of integers starting with one.
from1 :: [Int]
from1 = [0..]

-- | Generate all of the word lists used by the algorithm.
generateWordLists :: FilePath  -- ^ data directory name
                  -> IO ()
generateWordLists dataDirectory =
  do
    english <- getRankedEnglish dataDirectory
    (surnames, maleNames, femaleNames) <- getRankedCensusNames dataDirectory
    passwords <- getRankedCommonPasswords dataDirectory

    let
      englishMap = fromList $ zip english from0
      surnamesMap = fromList $ zip surnames from0
      maleNamesMap = fromList $ zip maleNames from0
      femaleNamesMap = fromList $ zip femaleNames from0
      passwordsMap = fromList $ zip passwords from0

    putStrLn "{-# LANGUAGE OverloadedStrings #-}\n"
    putStrLn
      "{- This file is machine-generated. Any changes may be ovewritten. -}\n"
    putStrLn "module Words where\n"

    putStrLn "import Codec.Compression.GZip ( decompress )"
    putStrLn "import Data.ByteString.Lazy.Char8 ( ByteString )"
    putStrLn "import qualified Data.Binary as Binary ( Binary, decode )"
    putStrLn "import qualified Data.ByteString.Base64.Lazy as Base64 ( decode )"
    putStrLn "import qualified Data.Either as E ( either )"
    putStrLn "import qualified Data.Map as M ( fromList, Map )"
    putStrLn "import qualified Data.Text as T ( pack, Text )"

    putStrLn ""

    putStrLn
      "deserialize :: Binary.Binary a => ByteString -> a\n\
      \deserialize text =\n\
      \  E.either (error . show) (Binary.decode . decompress) $\
      \ Base64.decode text"

    putStrLn ""

    TIO.putStrLn $ writeMap "dvorak" $
      buildGraph dvorak True

    putStrLn ""

    TIO.putStrLn $ writeMap "keypad" $
      buildGraph keypad False

    putStrLn ""

    TIO.putStrLn $ writeMap "macKeypad" $
      buildGraph macKeypad False

    putStrLn ""

    TIO.putStrLn $ writeMap "qwerty" $
      buildGraph qwerty True

    putStrLn ""

    TIO.putStrLn $ writeList "english" $
      filterDup english englishMap
        [surnamesMap, maleNamesMap, femaleNamesMap, passwordsMap]

    putStrLn ""

    TIO.putStrLn $ writeList "femaleNames" $
      filterDup femaleNames femaleNamesMap
        [englishMap, surnamesMap, maleNamesMap, passwordsMap]

    putStrLn ""

    TIO.putStrLn $ writeList "maleNames" $
      filterDup maleNames maleNamesMap
        [englishMap, surnamesMap, femaleNamesMap, passwordsMap]

    putStrLn ""

    TIO.putStrLn $ writeList "passwords" $
      filterDup passwords passwordsMap
        [englishMap, surnamesMap, maleNamesMap, femaleNamesMap]

    putStrLn ""

    TIO.putStrLn $ writeList "surnames" $
      filterDup surnames surnamesMap
        [englishMap, maleNamesMap, femaleNamesMap, passwordsMap]

    return ()

{- | Returns the nine clockwise adjacent coordinates on a keypad, where each
  row is vertically aligned.
-}
getAlignedAdjacentCoords :: Int           -- x coordinate of key
                         -> Int           -- y coordinate of key
                         -> [(Int, Int)]  -- coordinates of adjacent keys
getAlignedAdjacentCoords x y =
  [ (x - 1, y)
  , (x - 1, y - 1)
  , (x, y - 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  , (x, y + 1)
  , (x - 1, y + 1)
  ]

{- | Takes name lists from the the 2000 us census, prepares as lists in order
  of frequency (most common names first).

  More info:
  http://www.census.gov/genealogy/www/data/2000surnames/index.html

  Files in data are downloaded copies of:
  http://www.census.gov/genealogy/names/dist.all.last
  http://www.census.gov/genealogy/names/dist.male.first
  http://www.census.gov/genealogy/names/dist.female.first
-}
getRankedCensusNames :: FilePath  -- ^ data directory name
                     -> IO ([T.Text], [T.Text], [T.Text])
                        -- ^ (surnames, maleNames, femaleName)
getRankedCensusNames dataDirectory = do
    surnames <- do
      contents <-
        liftM (T.lines . T.pack) $ readFile $
          printf censusTempl ("surnames" :: String)
      return $
        filtered $
        map (\(name, _) -> T.toLower name) $
        takeWhile (\(_, frequency) -> frequency < surnameCutoffPercentile) $
        map matchLine contents
    maleNames <- liftM filtered $ readCensusFile "male_first"
    femaleNames <- liftM filtered $ readCensusFile "female_first"

    return (surnames, maleNames, femaleNames)
  where
    censusTempl = dataDirectory ++ "/us_census_2000_%s.txt"

    linePattern =
      "([A-Za-z]+)\\s+[0-9]+\\.[0-9]*\\s+([0-9]+\\.[0-9]*)\\s+[0-9]+" :: String

    matchLine :: T.Text -> (T.Text, Double)
    matchLine line =
      let
        [_, name, frequency] =
          getAllTextSubmatches
              (T.unpack line =~ linePattern :: (AllTextSubmatches [] String))
      in
        (T.pack name, read frequency :: Double)

    readCensusFile :: T.Text -> IO [T.Text]
    readCensusFile suffix = do
      contents <-
        liftM (T.lines . T.pack) $ readFile $ printf censusTempl $
          T.unpack suffix
      return $ map (T.toLower . fst . matchLine) contents

    surnameCutoffPercentile = 85  -- ^ ie7 can't handle huge lists - cut surname
                                  -- list off at a certain percentile

getRankedCommonPasswords:: FilePath     -- ^ data directory name
                        -> IO [T.Text]  -- ^ return list of common passwords in
                                        -- order by frequency
getRankedCommonPasswords dataDirectory = do
    contents <- liftM (T.lines . T.pack) $ readFile commonPasswords
    return $ filtered $ filter (not . T.null) contents
  where
    commonPasswords = dataDirectory ++ "/common_passwords.txt"

{- | Wikitionary has a list of ~40k English words, ranked by frequency of
  occurance in TV and movie transcripts.

  More details at:
  http://en.wiktionary.org/wiki/Wiktionary:Frequency_lists/TV/2006
    /explanation

  The list is separated into pages of 1000 or 2000 terms each.
  - the first 10k words are separated into pages of 1000 terms each
  - the remainder is separated into pages of 2000 terms each
-}
getRankedEnglish :: FilePath     -- ^ data directory name
                 -> IO [T.Text]  -- ^ return list of English words in order by
                                 --  frequency
getRankedEnglish dataDirectory = do
  let
    tvListUrl =
      "http://en.wiktionary.org/wiki/" ++
        "Wiktionary:Frequency_lists/TV/2006/%s"
    englishTempl = dataDirectory ++ "/tv_and_movie_freqlist%s.html"

    -- Create a list of index suffixes (e.g. "1-1000", "1001-2000", ...).
    indices =
        map format1 [0..9] ++ map format2 [0..14] ++ ["40001-41284" :: String]
      where
        format1 index =
          printf "%d-%d" ((index * 1000 + 1) :: Int)
                         (((index + 1) * 1000) :: Int)
        format2 index =
          printf "%d-%d" ((10000 + 2 * index * 1000 + 1) :: Int)
                         ((10000 + (2 * index + 2) * 1000) :: Int)
    wordFiles = map (printf englishTempl) indices
    urls = map (printf tvListUrl) indices

  -- Determine which of the word files are missing.
  missing <-
    let
      checkExists (filePath, _) = do
        exists <- doesFileExist filePath
        return $ not exists
    in
      filterM checkExists $ zip wordFiles urls

  -- Download the missing files from wiktionary to the data directory.
  forM_ missing $ \(filePath, url) -> do
    let
      request =
        Request
          { rqURI = fromJust $ parseURI url  -- URI assumed well-formed
          , rqMethod = GET
          , rqHeaders = [Header HdrUserAgent "zxcvbn"]
          , rqBody = ""
          }
    hPutStrLn stderr $ "Downloading " ++ show url
    resp <- simpleHTTP request
    case resp of
      Left _ -> hPutStrLn stderr $ "Error connecting to " ++ show url
      Right response -> do
        let
          html = rspBody response
        file <- openFile filePath WriteMode
        hPutStr file html
        hClose file
    return ()

  -- Parse the wiktionary html files to extract the words in order frequency.
  rankedTerms <-
    forM wordFiles $ \filePath -> do
      contents <- liftM T.pack $ readFile filePath
      return $ parseWikiTerms filePath contents

  return $ filtered $ concat rankedTerms

{- | Returns the six adjacent coordinates on a standard keyboard, where each
  row is slanted to the right from the last.
  Adjacencies are clockwise, starting with key to the left, then two keys
  above, then right key, then two keys below.
  (that is, only near-diagonal keys are adjacent, so g's coordinate is adjacent
  to those of t,y,b,v, but not those of r,u,n,c.)
-}
getSlantedAdjacentCoords :: Int           -- x coordinate of key
                         -> Int           -- y coordinate of key
                         -> [(Int, Int)]  -- coordinates of adjacent keys
getSlantedAdjacentCoords x y =
  [ (x - 1, y)
  , (x, y - 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x, y + 1)
  , (x - 1, y + 1)
  ]

-- | Search a Text for a substring.
indexOf :: T.Text     -- needle
        -> T.Text     -- haystack
        -> Maybe Int  -- Maybe index if found
indexOf needle haystack =
  let
    substrings = T.breakOnAll needle haystack
  in
    case substrings of
      [] -> Nothing
      (x:_) -> Just $ T.length $ fst x

keypad :: T.Text
keypad =
  "  / * -\n\
  \7 8 9 +\n\
  \4 5 6\n\
  \1 2 3\n\
  \  0 ."

macKeypad :: T.Text
macKeypad =
  "  = / *\n\
  \7 8 9 -\n\
  \4 5 6 +\n\
  \1 2 3\n\
  \  0 ."

main :: IO ()
main = do
  args <- getArgs

  -- The first argument is the dataDirectory name.
  case args of
    [dataDirectory] -> generateWordLists dataDirectory
    _ -> error "Usage: BuildDataFiles dataDirectory"

qwerty :: T.Text
qwerty =
  "`~ 1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0) -_ =+\n\
  \    qQ wW eE rR tT yY uU iI oO pP [{ ]} \\|\n\
  \     aA sS dD fF gG hH jJ kK lL ;: '\"\n\
  \      zZ xX cC vV bB nN mM ,< .> /?"

-- | Parse the wiki terms from each html file (terms in <tr><td><a>...).
parseWikiTerms :: FilePath  -- ^ file name of the html file
               -> T.Text    -- ^ html contents of the file
               -> [T.Text]  -- ^ return a list of words from ...<tr><td><a>...
parseWikiTerms filePath html =
    let
      rootElement =
        docContent (posInNewCxt filePath Nothing) $ htmlParse filePath $
          T.unpack html
      terms = (deep $ tag "tr" /> tag "td" /> tag "a" /> txt) rootElement
    in
      map (T.pack . render . content) terms

--deserialize :: Binary a => ByteString -> a
--deserialize text =
--  E.Either (error . show) (Binary.decode . decompress) $ Base64.decode

-- | Serialize a structure in abase64-encoded, gzip-compressed Text broken into
-- 64 character chunks.
serialize :: Binary a
          => a         -- the item to serialize
          -> [T.Text]  -- return the chunked, serialized value
serialize value =
  T.chunksOf 64 $ T.pack $ L.unpack $ B.encode $ compress $ encode value

{- | Generate a Haskell definition for a List identifier (e.g.: english) as a
  base64-encoded, gzip-compressed Text.
-}
writeList :: T.Text    -- ^ name of the identifier (e.g.: "english")
          -> [T.Text]  -- ^ terms to include in the identifier list
          -> T.Text    -- ^ Text containing the definition of the identifier
writeList name termList =
    let
      split = serialize $ map T.unpack termList
    in
      name <> " :: [T.Text]\n" <> name <> " =\n" <>
        "  map T.pack $ deserialize\n" <> quoteChunks 4 split
  where
    -- | Quote a list of Text chunks as one multiline Text
    quoteChunks :: Int       -- indent amount
                -> [T.Text]  -- chunks to be quoted
                -> T.Text    -- return the quoted chunks as one multiline Text
    quoteChunks indentAmount chunks =
      let
        indent = T.pack $ replicate indentAmount ' '
      in
        indent <> "\"" <> T.intercalate ("\\\n" <> indent <> "\\") chunks <>
          "\""

-- | Generate a Haskell definition for a Map identifier (e.g.: qwerty).
writeMap :: T.Text               -- ^ name of the identifier (e.g.: "qwerty")
         -> Map T.Text [T.Text]  -- ^ the adjacency Map to emit
         -> T.Text               -- ^ the Haskell definition of the Map
writeMap name adjacencyMap =
    name <> " :: M.Map T.Text [T.Text]\n" <> name <> " = M.fromList\n  [ " <>
      T.intercalate ("\n" <> indent <> ", ") (map keyValueToString $
        toList adjacencyMap) <> "\n" <> indent <> "]"
  where
    indent = "  "

    keyValueToString :: (T.Text, [T.Text]) -> T.Text
    keyValueToString (key, value) =
      "(" <> T.pack (show key) <> ", [" <>
        T.intercalate ", " (map (T.pack . show) value) <> "])"