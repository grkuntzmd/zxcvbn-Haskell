module Zxcvbn where

import Data.Map ( findWithDefault
                , foldrWithKey
                , fromList
                , Map
                , toList
                )
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
  , dmExtra :: DictionaryMatchExtra
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
    { leL33t :: Boolean
    , leSub :: M.Map Char Char
    , leSubDisplay :: T.Text
    }
  | RepeatExtra
    { reRepeatedChar :: Char
    }
  | SequenceExtra
    { seqAscending :: Boolean
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

l33tTable :: M.Map Char [Char]
l33tTable = fromList [
('a' -> List('4', '@')),
'b' -> List('8'),
'c' -> List('(', '{', '[', '<'),
'e' -> List('3'),
'g' -> List('6', '9'),
'i' -> List('1', '!', '|'),
'l' -> List('1', '|', '7'),
'o' -> List('0'),
's' -> List('$', '5'),
't' -> List('+', '7'),
'x' -> List('%'),
'z' -> List('2'))
  ]