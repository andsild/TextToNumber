module Parser where

import qualified Numbers
import Tokenizer

import qualified Data.Map as DM
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils


interpreter :: Stmt -> Int -> String
interpreter (GivenNumber number stmt) acc
  | acc == 0 = interpreter stmt $ parsedNumber
  | otherwise = interpreter stmt $ calculateNumber parsedNumber acc 
  where
    parsedNumber = translate number
interpreter (NonNumericString word stmt) acc = word ++ " " ++ interpreter stmt 0
interpreter Nil x = show x

calculateNumber :: Int -> Int -> Int
calculateNumber x prev
  | prev > 0 && prev <  x =  prev * x 
  | prev > 0 && prev >  x =  prev + x 
  | otherwise = x


translate :: UnitStringNumber -> Int
translate (IntegerNumber num) = num
translate (StringNumber num) = fromJust $ Numbers.elemIndex' num
