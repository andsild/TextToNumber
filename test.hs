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

-- TODO: this is not a CFG, its just string translation
-- review roman numerals, they have CFGs.

-- This program works for normal sentences that only has one number but could be improved a great deal
-- Its one of my first haskell programs which is why im sort of playing around with syntax

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr | Binary Stmt Op Stmt | Unary Stmt
  deriving Show
data Unop = Not deriving Show
data Join = And deriving Show
data Op = Minus | Plus deriving Show
data Duop = Band | Iff deriving Show
data Count = Number Integer | Unit String deriving Show
data Stmt = Seq [Stmt] | UnitNumber Count Stmt | Join Stmt
          deriving Show

unitNumbers :: [String]
unitNumbers = ["zero", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine", "ten", "eleven", "twelve", "thirteen", 
              "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
              "nineteen"]

scaleNumbers :: [String]
scaleNumbers = ["zero", "ten", "twenty", "thirty", "forty", "fifty"]

bigNumbers :: [String]
bigNumbers = ["zero", "hundred", "thousand", "million"]

elemIndex' :: String -> Integer
elemIndex' s = toInteger num
  where
    unitNumber = fromMaybe 0 $ elemIndex s unitNumbers
    scaleNumber = 10 * fromMaybe 0 (elemIndex s scaleNumbers)
    bigNumberResult = 1 + fromMaybe 0 (elemIndex s bigNumbers)
    bigNumber = if bigNumberResult == 1 then 0 else 10 ^ bigNumberResult
    num = maximum [unitNumber, scaleNumber, bigNumber]

def :: LanguageDef st
def = emptyDef{ identStart =  alphaNum
              , identLetter = alphaNum
              , opStart = oneOf "-"
              , opLetter = oneOf "-"
              , reservedOpNames = ["-"]
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

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table :: [[Operator String u Identity Expr]]
table = [ [Prefix (stmtReservedOp "~" >> return (Uno Not))]
        , [Infix (stmtReservedOp "&" >> return (Duo Band)) AssocLeft]
        , [Infix (stmtReservedOp "=" >> return (Duo Iff)) AssocLeft]
        , [Infix (stmtReservedOp "-"   >> return (Duo Iff)) AssocLeft]
        ]

term :: ParsecT String u Identity Expr
term = fmap Var stmtIdentifier 

returnToken :: Integer -> Stmt -> Stmt
returnToken num = UnitNumber (Number num)

convert :: String -> Stmt -> Stmt
convert s = UnitNumber (Unit s)

mainparser :: Parser Stmt
mainparser = stmtWhitespace >> stmtparser CA.<* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (stmtSemiSep stmt1)
      stmt1 = do { num <- stmtNatural 
                       ; e <- stmtparser
                    ; return (returnToken num e)
                    }
              <|> do { num <- stmtIdentifier
                       ; e <- stmtparser
                    ; return (convert num e)
                    }
              <|> do { stmtReservedOp "and"
                     ; e <- stmtparser
                     ; return (Join e)
              }
              <|> do { stmtReservedOp "-"
                     ; e <- stmtparser
                     ; return (Join e)
              }

valueOf :: Stmt -> [Integer]
valueOf stmt = case stmt of
  (Seq a) -> recurseOverStatements a
  (Join a) -> valueOf a
  UnitNumber (Number num) (Seq a) -> num : recurseOverStatements a
  UnitNumber (Unit num) (Seq a) -> elemIndex' (map toLower num)  : recurseOverStatements a

recurseOverStatements :: [Stmt] -> [Integer]
recurseOverStatements [] = []
recurseOverStatements [x] = valueOf x
recurseOverStatements (x:xs) = join [valueOf x,recurseOverStatements xs]

interpreter :: Stmt -> Integer
interpreter (Seq a) = calculateNumber (recurseOverStatements a) 0
                     -- sum (recurseOverStatements a)

calculateNumber :: [Integer] -> Integer -> Integer
calculateNumber [] _ = 0
calculateNumber [x] prev
  | prev > 0 && prev <  x =  prev * x 
  | prev > 0 && prev >  x =  prev + x 
  | otherwise = x
calculateNumber (x:xs) prev
  | prev > 0 && prev > x = calculateNumber xs $ prev + x -- e.g. twenty two hundred
  | prev > 0 && prev < x = prev * x + calculateNumber xs 0 -- e.g. two hundred
  | otherwise = calculateNumber xs (x + prev) -- e.g. hundred

main :: IO ()
main = do
  c <- getContents
  case parse mainparser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> print $ interpreter r
