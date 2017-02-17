module Parser where

import qualified Numbers
import Tokenizer

import qualified Data.Map as DM
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils

-- combineGivenNumbers :: (GivenNumber a _) -> GivenNumber (a _) -> Either a (a, a)
-- combineGivenNumbers l r = do
--   -- if either is fractional, don't combine
--   -- if right is < 100, don't combine as integers, just as string
--   -- if right is not exactly some power of 10 > 2, dont combine as integers, just as string
--   case l' of 
--     (IntegerNumber num) -> 

interpreter :: (Fractional a, Ord a, Show a) => Stmt -> a -> String
interpreter (GivenNumber number stmt) acc
  | parsedNumber == 0 && acc == 0 = "0 " ++ interpreter stmt 0
  | parsedNumber == 0 && not (acc == 0) = show acc ++ " 0 " ++ interpreter stmt 0
  | acc == 0 = interpreter stmt $ parsedNumber
  | otherwise = interpreter stmt $ calculateNumber parsedNumber acc
  where
    parsedNumber = translate number
interpreter (Combiner stmt) acc
  | acc == 0 = "then "
  | otherwise = show acc ++ interpreter stmt 0
interpreter (NonNumericString word stmt) acc
  | acc == 0 = word ++ " " ++ interpreter stmt 0
  | otherwise = show acc ++ " " ++ word ++ " " ++ interpreter stmt 0
interpreter Nil acc
  | acc == 0 = ""
  | otherwise = show acc

calculateNumber :: (Ord a, Fractional a) => a -> a -> a
calculateNumber x prev
  | prev > 0 && prev <  x =  prev * x
  | prev > 0 && prev >  x =  prev + x
  | otherwise = x

translate :: (Fractional a) => UnitStringNumber -> a
translate (IntegerNumber num) = fromIntegral num
translate (FractionalNumber num) = realToFrac num
translate (StringNumber num) = fromIntegral . fromJust $ Numbers.elemIndex' num
