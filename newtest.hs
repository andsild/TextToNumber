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

unitStringNumbers :: [String]
unitStringNumbers = ["zero", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine", "ten", "eleven", "twelve", "thirteen", 
              "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
              "nineteen"]

scaleStringNumbers :: [String]
scaleStringNumbers = ["zero", "ten", "twenty", "thirty", "forty", "fifty"]

bigStringNumbers :: [String]
bigStringNumbers = ["zero", "hundred", "thousand", "million"]


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
    stringacc = if x > -1 then "" else show acc
    newacc = if x > -1 then acc + x else 0
interpreter Nil x = show x


translate :: UnitStringNumber -> Integer
translate (IntegerNumber num) = num
translate (StringNumber num) = 9001

main :: IO ()
main = do
  c <- getContents
  case parse mainparser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> print $ interpreter r 0
