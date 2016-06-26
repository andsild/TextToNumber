import qualified Control.Applicative as CA
import Control.Monad
import Data.Char
import Data.List
import Data.Functor.Identity
import Data.Maybe
import Text.Parsec 
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

unitNumbers :: [String]
unitNumbers = ["zero", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine", "ten", "eleven", "twelve", "thirteen", 
              "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
              "nineteen"]

scaleNumbers :: [String]
scaleNumbers = ["zero", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

bigNumbers :: [String]
bigNumbers = ["zero", "hundred", "thousand", "million"]

elemIndex' :: String -> Integer
elemIndex' s = toInteger num
  where
    defaultNumber = -1
    unitNumber = fromMaybe defaultNumber $ elemIndex s unitNumbers
    scaleNumber = 10 * fromMaybe defaultNumber (elemIndex s scaleNumbers)
    bigNumberResult = 1 + fromMaybe defaultNumber (elemIndex s bigNumbers)
    bigNumber = if bigNumberResult == 0 then defaultNumber else 10 ^ bigNumberResult
    num = maximum [unitNumber, scaleNumber, bigNumber]


data Stmt = GivenNumber UnitStringNumber Stmt | Nil
          deriving Show
data UnitStringNumber = StringNumber String  | IntegerNumber Integer
          deriving Show



def :: LanguageDef st
def = emptyDef{ identStart =  alphaNum
              , identLetter = alphaNum 
              , opStart = oneOf "-"
              , opLetter = oneOf "-"
              , reservedOpNames = ["one", "-"]
              , reservedNames = ["and", "-"]
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
                       ; rest <- stmtparser
                        ; return ((GivenNumber (IntegerNumber num)) rest)
                    }
                    <|> do { num <- stmtIdentifier
                      ; rest <- stmtparser
                        ; return (GivenNumber (StringNumber num) rest)
                    }
                    <|> return Nil


interpreter :: Stmt -> Integer -> String
interpreter (GivenNumber unit stmt) acc = stringacc ++ interpreter stmt newacc
  where
    x = translate unit
    stringacc = if x > -1 then " " else accOut ++ readStringNumber unit
    accOut = if acc > 0 then show acc ++ " " else ""
    newacc = if x > -1 then calculateNumber x acc else 0
interpreter Nil x = if x > 0 then show x else " "

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
translate (StringNumber num) = elemIndex' (map toLower num)

main :: IO ()
main = do
  c <- getContents
  case parse mainparser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> print $ interpreter r 0
