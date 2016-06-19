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
scaleNumbers = ["zero", "ten", "twenty", "thirty", "forty", "fifty"]

bigNumbers :: [String]
bigNumbers = ["zero", "hundred", "thousand", "million"]


data Stmt = GivenNumber UnitNumber Stmt | Nil
          deriving Show
data UnitNumber = Number String  | UnitInteger Integer
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
                        ; return ((GivenNumber (UnitInteger num)) rest)
                    }
                    <|> do { num <- stmtIdentifier
                      ; rest <- stmtparser
                        ; return (GivenNumber (Number num) rest)
                    }
                    <|> do { return Nil
                    }


interpreter :: Stmt -> Integer
interpreter (GivenNumber unit stmt) = x + interpreter stmt
  where
    x = translate unit
interpreter Nil = 0


translate :: UnitNumber -> Integer
translate (UnitInteger num) = num
translate (Number num) = 9001







main :: IO ()
main = do
  c <- getContents
  case parse mainparser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> print $ interpreter r

