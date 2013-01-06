{-# LANGUAGE OverloadedStrings #-}

module Zxcvbn where

import Data.Char ( isAsciiLower
                 , isAsciiUpper
                 , isDigit
                 )
import Data.Int ( Int64 )
import qualified Data.List as L ( foldl' )
import Data.Map ( fromList
                , Map
                , size
                )
import qualified Data.Map as M ( foldl'
                               , lookup
                               )
import Data.Maybe ( fromJust )
import qualified Data.Text as T ( foldl'
                                , null
                                , Text
                                , unpack
                                )
import Text.Regex.PCRE ( (=~) )
import Text.Regex.Base.RegexLike ( AllMatches(getAllMatches) )
import Words ( dvorak
             , english
             , femaleNames
             , keypad
             , macKeypad
             , maleNames
             , passwords
             , qwerty
             , surnames
             )

data DictionaryMatch =
  DictionaryMatch
  { dmDictionaryName :: T.Text
  , dmEntropy :: Double
  , dmExtra :: Maybe DictionaryMatchExtra
  , dmI :: Int
  , dmJ :: Int
  , dmMatchedWord :: T.Text
  , dmPattern :: PatternType
  , dmRank :: Int
  , dmToken :: T.Text
  }

data DictionaryMatchExtra
  = DateExtra
    { deDay :: T.Text
    , deMonth :: T.Text
    , deSeparator :: T.Text
    , deYear :: T.Text
    }
  | L33tExtra
    { leL33t :: Bool
    , leSub :: Map Char Char
    , leSubDisplay :: T.Text
    }
  | RepeatExtra
    { reRepeatedChar :: Char
    }
  | SequenceExtra
    { seqAscending :: Bool
    , seqLength :: Int
    , seqName :: T.Text
    }
  | SpatialExtra
    { spaGraph :: T.Text
    , spaShiftedCount :: Int
    , spaTurns :: Int
    }

data PatternType
  = BruteForce
  | Date
  | Dictionary
  | Digits
  | Repeat
  | Sequence
  | Spatial
  | Year

type MatchList = [DictionaryMatch]

-- | Map from term to its one-based rank.
type RankedDictionary = Map T.Text Int

type Matcher = T.Text -> MatchList

l33tTable :: Map Char String
l33tTable = fromList
  [ ('a', "4@")
  , ('b', "8")
  , ('c', "({[<")
  , ('e', "3")
  , ('g', "69")
  , ('i', "1!|")
  , ('l', "1|7")
  , ('o', "0")
  , ('s', "$5")
  , ('t', "+7")
  , ('x', "%")
  , ('z', "2")
  ]

log2 :: Double
log2 = log 2

-- | Maximum days in a month.
numDays :: Int
numDays = 31

-- | Months in a year.
numMonths :: Int
numMonths = 12

-- | Years match against 1900 - 2019.
numYears :: Int
numYears = 119

-- | Match word containing no upper case.
allLowerPattern :: String
allLowerPattern = "^[^A-Z]+$"

-- | Match word containing no lower case.
allUpperPattern :: String
allUpperPattern = "^[^a-z]+$"

-- | Match a date without separators (e.g.: YYYYMMDD)
dateWithoutSepPattern :: String
dateWithoutSepPattern = "\\d{4,8}"

-- | Year, separator, day or month, same separator, month or day.
dateYearPrefixPattern :: String
dateYearPrefixPattern =
  "(19\\d{2}|20[01]\\d|\\d{2})(\\s|-|/|\\\\|_|\\.)(\\d{1,2})\\2(\\d{1,2})"

-- | Day or month, separator, month or day, same separator, year
dateYearSuffixPattern :: String
dateYearSuffixPattern =
  "(\\d{1,2})(\\s|-|/|\\\\|_|\\.)(\\d{1,2})\\2(19\\d{2}|20[01]\\d|\\d{2})"

-- | 3 or more digits.
digitsPattern :: String
digitsPattern = "\\d{3,}"

-- | All upper case.
endUpperPattern :: String
endUpperPattern = "^[^A-Z]+[A-Z]$"

-- | Leading upper case.
startUpperPattern :: String
startUpperPattern = "^[A-Z][^A-Z]+$"

-- | 19xx, 200x, 201x years.
yearPattern :: String
yearPattern = "19\\d\\d|20[01]\\d"

-- | Password attempts per second:
-- http://arstechnica.com/security/2012/12/
-- 25-gpu-cluster-cracks-every-standard-windows-password-in-6-hours/
attackRate :: Int64
attackRate = 350000000000

-- Number of seconds in each time unit.
secondsPerMinute :: Int
secondsPerMinute = 60

secondsPerHour :: Int
secondsPerHour = secondsPerMinute * 60

secondsPerDay :: Int
secondsPerDay = secondsPerHour * 24

secondsPerMonth :: Int
secondsPerMonth = secondsPerDay * 31

secondsPerYear :: Int
secondsPerYear = secondsPerMonth * 12

secondsPerCentury :: Int
secondsPerCentury = secondsPerYear * 100

-- | Keyboard adjacency data (keys next to other keys on the keyboard
-- (e.g.: qwerty))
adjacencyGraphs :: Map T.Text (Map T.Text [T.Text])
adjacencyGraphs = fromList
  [ ("dvorak", dvorak)
  , ("keypad", keypad)
  , ("macKeypad", macKeypad)
  , ("qwerty", qwerty)
  ]

-- | Functions that match words in each type of word list and return a List of
-- DictionaryMatch structures
dictionaryMatchers :: [Matcher]
dictionaryMatchers =
    [ buildMatcher "english"
    , buildMatcher "femaleNames"
    , buildMatcher "maleNames"
    , buildMatcher "passwords"
    , buildMatcher "surnames"
    ]
  where
    buildMatcher :: T.Text -> Matcher
    buildMatcher name =
      buildDictionaryMatcher name $ buildRankedDictionary $ fromJust $
        M.lookup name frequencyLists

-- | Ranked list of words (common passwords, common surnames, etc.).
frequencyLists :: Map T.Text [T.Text]
frequencyLists = fromList
  [ ("english", english)
  , ("femaleNames", femaleNames)
  , ("maleNames", maleNames)
  , ("passwords", passwords)
  , ("surnames", surnames)
  ]

-- | Average number of neighbors for each key on a standard keyboard.
keyboardAverageDegree :: Double
keyboardAverageDegree = undefined

-- | Number of keys that lead sequences of keys on a standard keyboard.
keyboardStartingPositions :: Int
keyboardStartingPositions =
  size $ fromJust $ M.lookup "qwerty" adjacencyGraphs

-- | Average number of neighbors for each key on a standard keypad.
keypadAverageDegree :: Double
keypadAverageDegree = undefined

-- | Number of keys that lead sequences of keys on a standard keypad.
keypadStartingPositions :: Int
keypadStartingPositions =
  size $ fromJust $ M.lookup "keypad" adjacencyGraphs

-- | All functions that match interesting sequences of characters.
matchers :: [Matcher]
matchers =
  dictionaryMatchers ++
  [ dateMatch
  , digitsMatch
  , l33tMatch
  , repeatMatch
  , sequenceMatch
  , spatialMatch
  , yearMatch
  ]

{- | Return a function that, given a password, returns a list of
  DictionaryMatch structures for all substrings of the password.
-}
buildDictionaryMatcher :: T.Text            -- ^ dictionary name
                       -> RankedDictionary  -- ^ term to rank Map
                       -> Matcher           -- ^ return the function
buildDictionaryMatcher name rankedDictionary password =
  dictionaryMatch name password rankedDictionary

{- | Given a list of words (dictionary), return a Map from the word to its
  1-based position.
-}
buildRankedDictionary :: [T.Text]          -- ^ List of unranked terms
                      -> RankedDictionary  -- ^ return Map from term to rank
buildRankedDictionary unrankedTerms = fromList $ zip unrankedTerms from1

-- | Calculate the average number of neighbors that each keyboard key has.
calculateAverageDegree :: Map T.Text [T.Text]
                       -> Double
calculateAverageDegree graph =
  fromIntegral (M.foldl' (\acc values ->
    acc + length (filter (not . T.null) values)) 0 graph) /
    fromIntegral (size graph)

{- | Given a password, calculate all of the classes of characters (lower,
  upper, digits, symbols) contained in the password, then given that,
  calculate the character set size.
-}
calculateBruteForceCardinality :: T.Text -- ^ candidate password
                               -> Int    -- ^ return the character set size
calculateBruteForceCardinality password =
  let
    (lower, upper, digits, symbols) =
      T.foldl' (\(l, u, d, s) char ->
        case char of
          c | isAsciiLower c -> (True, u, d, s)
            | isAsciiUpper c -> (l, True, d, s)
            | isDigit c      -> (l, u, True, s)
            | otherwise      -> (l, u, d, True))
        (False, False, False, False) password
  in
    (if lower then 26 else 0) +
    (if upper then 26 else 0) +
    (if digits then 10 else 0) +
    (if symbols then 33 else 0)

{- | For a given DictionaryMatch, calculate the number of bits of entropy in
  that password substring.
-}
calculateEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to
                                     -- calculate entropy
                 -> DictionaryMatch  -- ^ DictionaryMatch updated with entropy
calculateEntropy matcher =
  if dmEntropy matcher > 0
  then matcher
  else
    let
      entropyFunction =
        case dmPattern matcher of
          BruteForce -> undefined
          Date -> dateEntropy
          Dictionary -> dictionaryEntropy
          Digits -> digitsEntropy
          Repeat -> repeatEntropy
          Sequence -> sequenceEntropy
          Spatial -> spatialEntropy
          Year -> yearEntropy
    in
      matcher { dmEntropy = entropyFunction matcher }

-- | Check that a given candidate date is a real date in the range 1900 - 2019.
checkDate :: Int  -- ^ day of month
          -> Int  -- ^ month
          -> Int  -- ^ year
          -> Bool
checkDate day month year =
  let
    (correctedDay, correctedMonth) =
      if 12 <= month && month <= 31 && day <= 12
      then (month, day)
      else (day, month)
  in
    correctedDay <= 31 && correctedMonth <= 12 && 1900 <= year && year <= 2019

{- | Calculate the number of combinations in `n` items taken `k` at a time,
  using Int64 intermediate values to avoid arithmetic overflow.
-}
choose :: Int64  -- ^ n
       -> Int64  -- ^ k
       -> Int64  -- ^ return number of combinations
choose n k
  | k > n     = 0
  | k == 0    = 1
  | otherwise =
    fst $ L.foldl' (\(items, r) d -> (items - 1, r * items `div` d))
      (n, 1 :: Int64) [1..k]

-- | Calculate the number of bits of entropy in a date substring.
dateEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to calculate
                                -- entropy
            -> Double           -- ^ return calculated entropy
dateEntropy matcher =
  case fromJust (dmExtra matcher) of
    DateExtra _ _ separator y ->
      logBase2 (fromIntegral $ numDays * numMonths *
        (if (read (T.unpack y) :: Int) < 100 then 100 else numYears)) +
        (if T.null separator then 2 else 0)  -- Add 2 bits for separator
                                             -- character.
    _ -> undefined

{- | Find all password substrings matching a date pattern (with and without
  separators).
-}
dateMatch :: Matcher
dateMatch password = dateSepMatch password ++ dateWithoutSepMatch password
  where
    -- | Find all password substrings matching a date pattern with separators.
    dateSepMatch :: Matcher
    dateSepMatch pw =
      let
        matches =
          -- where the year is at the end (DDMMYYYY)
          map (\bounds @ (start, end) ->
            undefined) $ findAll password dateYearSuffixPattern
      in
        undefined

    -- | Find all password substrings matching a date pattern without
    -- separators.
    dateWithoutSepMatch :: Matcher
    dateWithoutSepMatch pw = undefined

{-- | Words in a word list have an entropy of log2(rank) + their upper case
  entropy + their l33t entropy.
-}
dictionaryEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to
                                      -- calculate entropy
                  -> Double           -- ^ return calculated entropy
dictionaryEntropy matcher = undefined

dictionaryMatch :: T.Text -- ^ dictionary name
                -> T.Text -- ^ candidate password
                -> RankedDictionary  -- ^ ranked dictionary
                -> [DictionaryMatch] -- ^ return matches
dictionaryMatch name password rankedDictionary = undefined

-- | Digit sequences have an entropy of log2(10 ^ (number of digits)).
digitsEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to calculate
                                  -- entropy
              -> Double           -- ^ return calculated entropy
digitsEntropy matcher = undefined

-- | Find all password substrings consisting of sequences of digits.
digitsMatch :: Matcher
digitsMatch password = undefined

-- | Find bounds (i, j) of all substrings of password that match the regex.
findAll :: T.Text  -- ^ password to find
        -> String  -- regex pattern to find in password
        -> [(Int, Int)]  -- ^ List of bounds of matches [start, end)
findAll password regex =
  map (\(start, len) -> (start, start + len)) $
    getAllMatches $ T.unpack password =~ regex

-- | Infinite list of integers starting with zero.
from0 :: [Int]
from0 = [0..]

-- | Infinite list of integers starting with one.
from1 :: [Int]
from1 = [0..]

-- | Find all L33t substitutions in the password.
l33tMatch :: Matcher
l33tMatch password = undefined

-- | Calculate the log base 2 of the input.
logBase2 :: Double  -- ^ value for log to be taken
         -> Double  -- ^ return the logarithm base 2
logBase2 n = log n / log2

-- | Calculate entropy of repeated characters.
repeatEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to calculate
                                  -- entropy
              -> Double           -- ^ return calculated entropy
repeatEntropy matcher = undefined

{- | Find repeating characters in the password
  Algorithm from http://stackoverflow.com/a/13651781/96233
-}
repeatMatch :: Matcher
repeatMatch password = undefined

{- | Calculate the entropy of consecutive letters or digits (ascending or
  descending)
-}
sequenceEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to
                                    -- calculate entropy
                -> Double           -- ^ return calculated entropy
sequenceEntropy matcher = undefined

-- | Find sequences of consecutive letters or digits (ascending or descending).
sequenceMatch :: Matcher
sequenceMatch password = undefined

-- | Calculate the entropy based on the adjacency of the keys on the keyboard.
spatialEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to calculate
                                   -- entropy
               -> Double           -- ^ return calculated entropy
spatialEntropy matcher = undefined

-- | Find sequences of characters that are on adjacent keys on the keyboard.
spatialMatch :: Matcher
spatialMatch password = undefined

-- | Calculate the entropy of a year value.
yearEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to calculate
                                -- entropy
            -> Double           -- ^ return calculated entropy
yearEntropy matcher = undefined

-- | Find sequences of characters that are years in the range 1900 - 2019.
yearMatch :: Matcher
yearMatch password = undefined
