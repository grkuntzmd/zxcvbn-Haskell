{-# LANGUAGE OverloadedStrings #-}

{-
Copyright 2013 G. Ralph Kuntz, MD

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

module Zxcvbn where

import Data.Char ( isAsciiLower
                 , isAsciiUpper
                 , isDigit
                 )
import Data.Int ( Int64 )
import Data.List ( find
                 , foldl'
                 )
import Data.Map ( empty
                , fromList
                , Map
                , member
                , size
                , (!)
                )
import qualified Data.Map as M ( foldl' )
import Data.Map.Lazy ( foldlWithKey' )
import Data.Maybe ( fromJust )
import qualified Data.Text as T ( drop
                                , filter
                                , foldl'
                                , length
                                , null
                                , pack
                                , take
                                , Text
                                , toLower
                                , unpack
                                )
import Data.Text.Read ( decimal )
import Text.Regex ( Regex )
import Text.Regex.Base.RegexLike ( AllMatches(getAllMatches)
                                 , makeRegex
                                 , matchTest
                                 )
import Text.Regex.PCRE ( (=~) )
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
  , dmStart :: Int
  , dmEnd :: Int
  , dmMatchedWord :: T.Text
  , dmPattern :: PatternType
  , dmRank :: Int
  , dmToken :: T.Text
  } deriving Show

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
  deriving Show

type Matcher = T.Text -> MatchList

type MatchList = [DictionaryMatch]

data PatternType
  = BruteForce
  | Date
  | Dictionary
  | Digits
  | Repeat
  | Sequence
  | Spatial
  | Year
  deriving Show

-- | Map from term to its one-based rank.
type RankedDictionary = Map T.Text Int

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
secondsPerMinute :: Double
secondsPerMinute = 60

secondsPerHour :: Double
secondsPerHour = secondsPerMinute * 60

secondsPerDay :: Double
secondsPerDay = secondsPerHour * 24

secondsPerMonth :: Double
secondsPerMonth = secondsPerDay * 31

secondsPerYear :: Double
secondsPerYear = secondsPerMonth * 12

secondsPerCentury :: Double
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
      buildDictionaryMatcher name $ buildRankedDictionary $
      frequencyLists ! name

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
keyboardAverageDegree = calculateAverageDegree qwerty

-- | Number of keys that lead sequences of keys on a standard keyboard.
keyboardStartingPositions :: Int
keyboardStartingPositions =
  size $ adjacencyGraphs ! "qwerty"

-- | Average number of neighbors for each key on a standard keypad.
keypadAverageDegree :: Double
keypadAverageDegree = calculateAverageDegree keypad

-- | Number of keys that lead sequences of keys on a standard keypad.
keypadStartingPositions :: Int
keypadStartingPositions =
  size $ adjacencyGraphs ! "keypad"

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
      T.foldl' (\(lower', upper', digits', symbols') char ->
        case char of
          c | isAsciiLower c -> (True, upper', digits', symbols')
            | isAsciiUpper c -> (lower', True, digits', symbols')
            | isDigit c      -> (lower', upper', True, symbols')
            | otherwise      -> (lower', upper', digits', True))
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
          BruteForce -> error "Unexpected BruteForce in pattern match"
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
    correctedDay <= 31 && correctedMonth <= 12 &&
    (1900 <= year && year <= 2019 || 1 <= year && year <= 99)

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
    fst $ foldl' (\(items, r) d -> (items - 1, r * items `div` d))
      (n, 1 :: Int64) [1 .. k]

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
    _ -> error "Unexpected pattern match (not DateExtra)"

{- | Find all password substrings matching a date pattern (with and without
  separators).
-}
dateMatch :: Matcher
dateMatch password = dateSepMatch ++ dateWithoutSepMatch
  where
    -- | Find all password substrings matching a date pattern with separators.
    dateSepMatch :: MatchList
    dateSepMatch =
      let
        suffixMatches =
          -- where the year is at the end (DDMMYYYY)
          map (\bounds @ (start, end) ->
            let
              [day, separator, month, year] =
                findGroupsIn (substring password start $ Just end)
                dateYearSuffixPattern
            in
              (day, month, year, separator, bounds)) $
                findAll password dateYearSuffixPattern
        prefixMatches =
          -- where the year is at the beginning (YYYYDDMM)
          map (\bounds @ (start, end) ->
            let
              [year, separator, month, day] =
                findGroupsIn (substring password start $ Just end)
                dateYearPrefixPattern
            in
              (day, month, year, separator, bounds)) $
                findAll password dateYearPrefixPattern
      in
        [ DictionaryMatch
          { dmDictionaryName = ""
          , dmEntropy = 0
          , dmExtra = Just $ DateExtra day month separator year
          , dmStart = start
          , dmEnd = end
          , dmMatchedWord = ""
          , dmPattern = Date
          , dmRank = 0
          , dmToken = substring password start $ Just end
          }
        | (day, month, year, separator, (start, end)) <-
            suffixMatches ++ prefixMatches
        , checkDate (textToInt day) (textToInt month) (textToInt year)
        ]

    -- | Find all password substrings matching a date pattern without
    -- separators.
    dateWithoutSepMatch :: MatchList
    dateWithoutSepMatch =
      let
        -- year at the beginning or end of date
        dayMonthYearSplit =
          concatMap (\bounds @ (start, end) ->
            let
              token = substring password start $ Just end
              len = T.length token
            in
              if len <= 6
              then [ -- 2-digit year prefix
                     ( substring token 2 Nothing  -- day-month
                     , substring token 0 $ Just 2  -- year
                     , bounds
                     )
                   , -- 2-digit year suffix
                     ( substring token 0 $ Just $ len - 2  -- day-month
                     , substring token (len - 2) Nothing  -- year
                     , bounds
                     )
                   , -- 4-digit year prefix
                     ( substring token 4 Nothing
                     , substring token 0 $ Just 4
                     , bounds
                     )
                   , -- 4-digit year suffix
                     ( substring token 0 $ Just 2
                     , substring token 2 Nothing
                     , bounds
                     )
                   ]
              else [ -- 4-digit year prefix
                     ( substring token 4 Nothing  -- day-month
                     , substring token 0 $ Just 4  -- year
                     , bounds
                     )
                   , -- 4-digit year suffix
                     ( substring token 0 $ Just $ len - 4  -- day-month
                     , substring token (len - 4) Nothing  -- year
                     , bounds
                     )
                   ]) $ findAll password dateWithoutSepPattern
        -- parse the day and month parts of the candidate date
        dayMonthSplit =
          concatMap (\(dayMonth, year, bounds) ->
            case T.length dayMonth of
              2 -> [ ( substring dayMonth 0 $ Just 1
                     , substring dayMonth 1 Nothing
                     , year
                     , bounds)
                   ]
              3 -> [ ( substring dayMonth 0 $ Just 2
                     , substring dayMonth 2 Nothing
                     , year
                     , bounds
                     )
                   , ( substring dayMonth 0 $ Just 1
                     , substring dayMonth 1 Nothing
                     , year
                     , bounds
                     )
                   ]
              4 -> [ ( substring dayMonth 0 $ Just 2
                     , substring dayMonth 2 Nothing
                     , year
                     , bounds
                     )
                   , ( substring dayMonth 2 Nothing
                     , substring dayMonth 0 $ Just 2
                     , year
                     , bounds
                     )
                   ]
              _ -> error "Unknown dayMonth length") dayMonthYearSplit
      in
        -- for each candidate, if it is a legal date in the range 1900 - 2019,
        -- include its substring
        [ DictionaryMatch
          { dmDictionaryName = ""
          , dmEntropy = 0
          , dmExtra = Just $ DateExtra day month "" year
          , dmStart = start
          , dmEnd = end
          , dmMatchedWord = ""
          , dmPattern = Date
          , dmRank = 0
          , dmToken = substring password start $ Just end
          }
        | (day, month, year, (start, end)) <- dayMonthSplit
        , checkDate (textToInt day) (textToInt month) (textToInt year)
        ]

    -- | Convert a Text to an Int
    textToInt :: T.Text -> Int
    textToInt = fst . either error id . decimal

{-- | Words in a word list have an entropy of log2(rank) + their upper case
  entropy + their l33t entropy.
-}
dictionaryEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to
                                      -- calculate entropy
                  -> Double           -- ^ return calculated entropy
dictionaryEntropy matcher = logBase2 (fromIntegral $ dmRank matcher) +
  extraUppercaseEntropy + extraL33tEntropy
  where
    extraUppercaseEntropy :: Double
    extraUppercaseEntropy =
      let
        word = T.unpack $ dmToken matcher
      in
        if matchTest (makeRegex allLowerPattern :: Regex) word
        then 0.0
        else
          -- check if any of the regexes match `word`
          let
            matches =
              any (\regex -> matchTest (makeRegex regex :: Regex) word)
              [allUpperPattern, endUpperPattern, startUpperPattern]
          in
            if matches
            then 1.0
            else
                let
                  upper = fromIntegral $ length $ filter isAsciiUpper word
                  lower = fromIntegral $ length $ filter isAsciiLower word
                in
                  foldl' (\result index ->
                    fromIntegral (choose (upper + lower) index) + result)
                    0.0 [0 .. (min upper lower)]

    extraL33tEntropy  :: Double
    extraL33tEntropy =
      let
        Just (L33tExtra l33t sub _) = dmExtra matcher
      in
        if not l33t
        then 0.0
        else
          let
            token = dmToken matcher
            possibilities =
              logBase2 $ foldlWithKey' (\result subbed unsubbed ->
                let
                  subbed' =
                    fromIntegral $ T.length $ T.filter (== subbed) token
                  unsubbed' =
                    fromIntegral $ T.length $ T.filter (== unsubbed) token
                  current =
                    foldl' (\result' index ->
                      fromIntegral $ choose (subbed' + unsubbed') index)
                      0.0 [0 .. (min subbed' unsubbed')]
                in
                  result + current) 0.0 sub
          in
            if possibilities > 0 then possibilities else 1.0

dictionaryMatch :: T.Text -- ^ dictionary name
                -> T.Text -- ^ candidate password
                -> RankedDictionary  -- ^ ranked dictionary
                -> [DictionaryMatch] -- ^ return matches
dictionaryMatch name password rankedDictionary =
  let
    len = T.length password
    passwordLower = T.toLower password
  in
    [ DictionaryMatch
      { dmDictionaryName = name
      , dmEntropy = 0
      , dmExtra = Just $ L33tExtra False empty ""
      , dmStart = start
      , dmEnd = end + 1
      , dmMatchedWord = word
      , dmPattern = Dictionary
      , dmRank = rankedDictionary ! word
      , dmToken = substring password start $ Just (end + 1)
      }
    | start <- [0 .. (len - 1)]
    , end <- [start .. (len - 1)]
    , let word = substring passwordLower start $ Just (end + 1)
    , member word rankedDictionary
    ]

-- | Digit sequences have an entropy of log2(10 ^ (number of digits)).
digitsEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to calculate
                                  -- entropy
              -> Double           -- ^ return calculated entropy
digitsEntropy matcher = logBase2 $ 10.0 ** dmEntropy matcher

-- | Find all password substrings consisting of sequences of digits.
digitsMatch :: Matcher
digitsMatch password = undefined

-- | Convert a number of seconds to a round number of larger units.
displayTime :: Double  -- ^ number of seconds
            -> T.Text  -- ^ return the display Text
displayTime seconds =
  case findUnits of
    Just (0.0, "instant") -> "instant"
    Just (limit, name) ->
      T.pack $ show (1 + ceiling (seconds / limit) :: Int) ++ " " ++ name
    Nothing -> "centuries"
  where
    findUnits =
      if seconds < secondsPerMinute
      then Just (0.0, "instant")
      else
        find (\(limit, _) -> seconds < limit)
        [ (secondsPerHour, "minutes")
        , (secondsPerDay, "hours")
        , (secondsPerMonth, "days")
        , (secondsPerYear, "months")
        , (secondsPerCentury, "years")
        ]

-- | Find bounds (i, j) of all substrings of password that match the regex.
findAll :: T.Text  -- ^ password to find
        -> String  -- regex pattern to find in password
        -> [(Int, Int)]  -- ^ List of bounds of matches (start closed, end open)
findAll password regex =
  map (\(start, len) -> (start, start + len)) $
    getAllMatches $ T.unpack password =~ regex

-- | Find all groups in Text matching a Regex.
findGroupsIn :: T.Text    -- ^ Text to search
             -> String    -- ^ regex pattern
             -> [T.Text]  -- ^ substrings found in regex groups (1, ...)
findGroupsIn text regex =
  case T.unpack text =~ regex :: (String, String, String, [String]) of
    (_, _, _, groups) -> map T.pack groups

-- | Infinite list of integers starting with zero.
from0 :: [Int]
from0 = [0..]

-- | Infinite list of integers starting with one.
from1 :: [Int]
from1 = [1..]

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

-- | Retrieve a substring of a Text. If the end parameter is Nothing, return
-- the substring from the start to the end of the string.
substring :: T.Text     -- ^ the Text to split
          -> Int        -- ^ zero-based offset of start of substring (closed)
          -> Maybe Int  -- ^ zero-based offset of end of substring (open)
          -> T.Text     -- ^ return the substring
substring string start maybeEnd =
  case maybeEnd of
    Just end -> T.take (end - start) dropBeginning
    Nothing -> dropBeginning
  where
    dropBeginning = T.drop start string

-- | Calculate the entropy of a year value.
yearEntropy :: DictionaryMatch  -- ^ DictionaryMatch for which to calculate
                                -- entropy
            -> Double           -- ^ return calculated entropy
yearEntropy matcher = undefined

-- | Find sequences of characters that are years in the range 1900 - 2019.
yearMatch :: Matcher
yearMatch password = undefined
