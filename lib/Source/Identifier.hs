module Source.Identifier
  ( Name
  , stringToName
  , nameToString
  , nameToText
  , alphabet
  , unsafeStringToName
  , Identifier
  , identifierZero
  , identifierToName
  , nameToIdentifier
  , named
  ) where

import Control.Lens
import Data.List as List
import Data.Serialize as Cereal
import Data.Text as Text
import Numeric.Natural
import Test.QuickCheck as QC

alphabet :: [Char]
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Invariant: non-empty and characters are in the alphabet
newtype Name = Name String
  deriving (Eq, Ord, Show)

instance Arbitrary Name where
  arbitrary = do
    len <- choose (1, 1000)
    unsafeStringToName <$>
      vectorOf len (QC.elements alphabet)

unsafeStringToName :: String -> Name
unsafeStringToName = Name

stringToName :: String -> Maybe Name
stringToName s
  | not (List.null s) && List.all (`List.elem` alphabet) s = Just (Name s)
  | otherwise = Nothing

nameToString :: Name -> String
nameToString (Name s) = s

nameToText :: Name -> Text
nameToText = Text.pack . nameToString

-- Invariant: n >= 0
newtype Identifier = Identifier Natural
  deriving (Eq, Ord, Enum, Show, Serialize, Arbitrary)

identifierZero :: Identifier
identifierZero = Identifier 0

toDigits :: Natural -> Natural -> [Int]
toDigits _    0 = []
toDigits base n = fromIntegral r : toDigits base q
  where
    (q, r) = quotRem n base

identifierToName :: Identifier -> Name
identifierToName (Identifier n) = Name (mkS 1 base n)
  where
    base = fromIntegral (List.length alphabet)
    mkS :: Int -> Natural -> Natural -> String
    mkS !k !base' !n' =
      if base' > n'
        then
          List.map (alphabet!!) . List.reverse . List.take k $
            toDigits base n' ++ List.repeat 0
        else
          mkS (k + 1) (base' * base) (n' - base')

nameToIdentifier :: Name -> Identifier
nameToIdentifier (Name s) =
  Identifier . (+offset) . sum . List.zipWith (*) bases . List.reverse $ ns
  where
    base = fromIntegral (List.length alphabet)
    bases = List.iterate (*base) 1
    ns = s <&> \c -> fromIntegral . List.head $ findIndices (== c) alphabet
    k = List.length ns
    offset = sum (List.take k bases) - 1

named :: Iso' Identifier Name
named = iso identifierToName nameToIdentifier
