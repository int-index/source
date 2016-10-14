module Source.Identifier
  ( Name
  , stringToName
  , nameToString
  , Identifier
  , identifierToName
  , nameToIdentifier
  ) where

import Data.Serialize as Cereal
import Data.List

alphabet :: [Char]
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Invariant: non-empty and characters are in the alphabet
newtype Name = Name String
  deriving (Eq, Ord, Show)

stringToName :: String -> Maybe Name
stringToName s
  | not (null s) && all (`elem` alphabet) s = Just (Name s)
  | otherwise = Nothing

nameToString :: Name -> String
nameToString (Name s) = s

-- Invariant: n >= 0
newtype Identifier = Identifier Int
  deriving (Eq, Ord, Enum, Show, Serialize)

instance Bounded Identifier where
  minBound = Identifier 0
  maxBound = Identifier maxBound

toDigits :: Int -> Int -> [Int]
toDigits _    0 = []
toDigits base n = r : toDigits base q
  where
    (q, r) = quotRem n base

identifierToName :: Identifier -> Name
identifierToName (Identifier n) = Name (mkS 1 base n)
  where
    base = length alphabet
    mkS !k !base' !n' =
      if base' > n'
        then
          map (alphabet!!) . reverse . take k $
            toDigits base n' ++ repeat 0
        else
          mkS (k + 1) (base' * base) (n' - base')

nameToIdentifier :: Name -> Identifier
nameToIdentifier (Name s) =
  Identifier . (+offset) .  sum . zipWith (*) bases . reverse $ ns
  where
    base = length alphabet
    bases = iterate (*base) 1
    ns = map (\c -> head $ findIndices (== c) alphabet) s
    k = length ns
    offset = sum (take k bases) - 1

