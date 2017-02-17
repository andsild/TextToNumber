module Parser where

import qualified Numbers
import Tokenizer

import qualified Data.Map as DM
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils

isPowerOfTen = 

-- How do I deal with 56 3 100?
-- How do I deal with 3 100 7 100
-- How do I deal with 30 100  100
-- make a list of all the numbers, traverse backwards
-- ... but I know 56 3 isnt combined
combineGivenNumbers :: (RealFrac f, Num a) 
  => (GivenNumber a _) 
  -> GivenNumber (a _) 
  -> Either f (f, f)
combineGivenNumbers (IntegerNumber i) (FractionNumber f) = Right (i, f)
combineGivenNumbers (FractionNumber f) (IntegerNumber i) = Right (f, i)
combineGivenNumbers (FractionNumber f1) (FractionNumber f2) = Right (f1, f2)
combineGivenNumbers (IntegerNumber i) (FractionNumber f) = Right (i, f)
combineGivenNumbers (IntegerNumber i1) (IntegerNumber i2)
  | i2 < 100 = Right (i1, i2)
  | (not $ i1 `elem` powersOfTen && i1 `elem` tenners) && i2 `elem` powersOfTen == i1 * i2
  | otherwise = 
    where
      -- logarithms are nice, but I can just do this too...
      powersOfTen = take 7 $ iterate (*10) 10
      tenners = take 9 $ iterate (+10) 10

  -- if either is fractional, don't combine
  -- if right is < 100, don't combine as integers, just as string
  -- if right is not exactly some power of 10 > 2, dont combine as integers, just as string
  case l' of 
    (IntegerNumber num) -> 

show' :: (RealFrac a, Show a) => a -> String
show' f = if isInt then show (round f) else show f
  where
    isInt = f == rounded
    rounded = fromInteger (round f)

interpreter :: (RealFrac a, Ord a, Show a) => Stmt -> a -> String
interpreter (GivenNumber number stmt) acc
  | parsedNumber == 0 && acc == 0 = "0 " ++ interpreter stmt 0
  | parsedNumber == 0 && not (acc == 0) = show' acc ++ " 0 " ++ interpreter stmt 0
  | acc == 0 = interpreter stmt $ parsedNumber
  | otherwise = interpreter stmt $ calculateNumber parsedNumber acc
  where
    parsedNumber = translate number
interpreter (Combiner stmt) acc
  | acc == 0 = "then "
  | otherwise = show' acc ++ interpreter stmt 0
interpreter (NonNumericString word stmt) acc
  | acc == 0 = word ++ " " ++ interpreter stmt 0
  | otherwise = show' acc ++ " " ++ word ++ " " ++ interpreter stmt 0
interpreter Nil acc
  | acc == 0 = ""
  | otherwise = show' acc

calculateNumber :: (Ord a, RealFrac a) => a -> a -> a
calculateNumber x prev
  | prev > 0 && prev <  x =  prev * x
  | prev > 0 && prev >  x =  prev + x
  | otherwise = x

translate :: (Fractional a) => UnitStringNumber -> a
translate (IntegerNumber num) = fromIntegral num
translate (FractionalNumber num) = realToFrac num
translate (StringNumber num) = fromIntegral . fromJust $ Numbers.elemIndex' num
