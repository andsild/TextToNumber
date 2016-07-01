module Main where

import qualified Control.Applicative as CA
import Data.Char
import qualified Data.Map as DM
import Data.List
import Data.String.Utils
import Data.Functor.Identity
import Data.Maybe
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

al :: [(String, Int)]
al = [ ("one", 1)
  ,("two", 2)
  ,("three", 3)
  ,("four", 4)
  ,("five", 5)
  ,("six", 6)
  ,("seven", 7)
  ,("eight", 8)
  ,("nine", 9)
  ,("ten", 10)
  ,("eleven", 11)
  ,("twelve", 12)
  ,("thirteen", 13)
  ,("fourteen", 14)
  ,("fifteen", 15)
  ,("sixteen", 16)
  ,("seventeen", 17)
  ,("eighteen", 18)
  ,("nineteen", 19)
  ,("twenty", 20)
  ,("twentyone", 21)
  ,("twentytwo", 22)
  ,("twentythree", 23)
  ,("twentyfour", 24)
  ,("twentyfive", 25)
  ,("twentysix", 26)
  ,("twentyseven", 27)
  ,("twentyeight", 28)
  ,("twentynine", 29)
  ,("thirty", 30)
  ,("thirtyone", 31)
  ,("thirtytwo", 32)
  ,("thirtythree", 33)
  ,("thirtyfour", 34)
  ,("thirtyfive", 35)
  ,("thirtysix", 36)
  ,("thirtyseven", 37)
  ,("thirtyeight", 38)
  ,("thirtynine", 39)
  ,("fourty", 40)
  ,("fourtyone", 41)
  ,("fourtytwo", 42)
  ,("fourtythree", 43)
  ,("fourtyfour", 44)
  ,("fourtyfive", 45)
  ,("fourtysix", 46)
  ,("fourtyseven", 47)
  ,("fourtyeight", 48)
  ,("fourtynine", 49)
  ,("forty", 40)
  ,("fortyone", 41)
  ,("fortytwo", 42)
  ,("fortythree", 43)
  ,("fortyfour", 44)
  ,("fortyfive", 45)
  ,("fortysix", 46)
  ,("fortyseven", 47)
  ,("fortyeight", 48)
  ,("fortynine", 49)
  ,("fifty", 50)
  ,("fiftyone", 51)
  ,("fiftytwo", 52)
  ,("fiftythree", 53)
  ,("fiftyfour", 54)
  ,("fiftyfive", 55)
  ,("fiftysix", 56)
  ,("fiftyseven", 57)
  ,("fiftyeight", 58)
  ,("fiftynine", 59)
  ,("sixty", 60)
  ,("sixtyone", 61)
  ,("sixtytwo", 62)
  ,("sixtythree", 63)
  ,("sixtyfour", 64)
  ,("sixtyfive", 65)
  ,("sixtysix", 66)
  ,("sixtyseven", 67)
  ,("sixtyeight", 68)
  ,("sixtynine", 69)
  ,("seventy", 70)
  ,("seventyone", 71)
  ,("seventytwo", 72)
  ,("seventythree", 73)
  ,("seventyfour", 74)
  ,("seventyfive", 75)
  ,("seventysix", 76)
  ,("seventyseven", 77)
  ,("seventyeight", 78)
  ,("seventynine", 79)
  ,("eighty", 80)
  ,("eightyone", 81)
  ,("eightytwo", 82)
  ,("eightythree", 83)
  ,("eightyfour", 84)
  ,("eightyfive", 85)
  ,("eightysix", 86)
  ,("eightyseven", 87)
  ,("eightyeight", 88)
  ,("eightynine", 89)
  ,("ninety", 90)
  ,("ninetyone", 91)
  ,("ninetytwo", 92)
  ,("ninetythree", 93)
  ,("ninetyfour", 94)
  ,("ninetyfive", 95)
  ,("ninetysix", 96)
  ,("ninetyseven", 97)
  ,("ninetyeigh", 98)
  ,("ninetynine", 99)
  ]

mapFromAL :: DM.Map String Int
mapFromAL = DM.fromList al

bigNumbers :: [String]
bigNumbers = ["zero", "hundred", "thousand", "million"]

elemIndex' :: String -> Integer
elemIndex' s = toInteger num
  where
    defaultNumber = -1
    unitOrScaleNumber = fromMaybe defaultNumber (DM.lookup s mapFromAL )
    -- unitNumber = fromMaybe defaultNumber $ elemIndex s unitNumbers
    -- scaleNumber = 10 * fromMaybe defaultNumber (elemIndex s scaleNumbers)
    bigNumberResult = 1 + fromMaybe defaultNumber (elemIndex s bigNumbers)
    bigNumber = if bigNumberResult == 0 then defaultNumber else 10 ^ bigNumberResult
    num = maximum [unitOrScaleNumber, bigNumber]


data Stmt = GivenNumber UnitStringNumber Stmt | Nil
          deriving Show
data UnitStringNumber = StringNumber String  | IntegerNumber Integer
          deriving Show

def :: LanguageDef st
def = emptyDef{ identStart =  alphaNum
              , identLetter = alphaNum <|> char '-'
              , reservedOpNames = ["and"]
              , reservedNames = []
              }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser def

stmtIdentifier :: ParsecT String u Identity String
stmtIdentifier = identifier lexer
stmtReservedOp :: String -> ParsecT String u Identity ()
stmtReservedOp = reservedOp lexer
stmtNatural :: ParsecT String u Identity Integer
stmtNatural    = natural lexer
stmtSemiSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
stmtSemiSep    = semiSep lexer
stmtWhitespace :: ParsecT String u Identity ()
stmtWhitespace = whiteSpace lexer

mainparser :: Parser Stmt
mainparser = stmtWhitespace >> stmtparser CA.<* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = do { num <- stmtNatural
                      ; skipMany (stmtReservedOp "and")
                       ; rest <- stmtparser
                        ; return (GivenNumber (IntegerNumber num) rest)
                    }
                    <|> do { num <- stmtIdentifier
                      ; skipMany (stmtReservedOp "and")
                      ; rest <- stmtparser
                        ; return (GivenNumber (StringNumber num) rest)
                    }
                    <|> return Nil


interpreter :: Stmt -> Integer -> String
interpreter (GivenNumber unit stmt) acc = stringacc ++ interpreter stmt newacc
  where
    x = translate unit
    stringacc = if x > -1 then "" else accOut ++ readStringNumber unit
    accOut = if acc > 0 then show acc ++ " " else ""
    newacc = if x > -1 then calculateNumber x acc else 0
interpreter Nil x = if x > 0 then show x else ""

readStringNumber :: UnitStringNumber -> String
readStringNumber (IntegerNumber num) = "meg"
readStringNumber (StringNumber num) = num ++ " "


calculateNumber :: Integer -> Integer -> Integer
calculateNumber x prev
  | prev > 0 && prev <  x =  prev * x 
  | prev > 0 && prev >  x =  prev + x 
  | otherwise = x


translate :: UnitStringNumber -> Integer
translate (IntegerNumber num) = num
translate (StringNumber num) = elemIndex' (Data.List.map toLower (replace "-" "" num))

main :: IO ()
main = do
  c <- getContents
  case parse mainparser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> putStrLn $  rstrip $  interpreter r 0

