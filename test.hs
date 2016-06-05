import qualified Control.Applicative as CA
import Data.List
import Data.Functor.Identity
import Data.Maybe
import Text.Parsec 
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr 
  deriving Show
data Unop = Not deriving Show
data Join = And deriving Show
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
    bigNumberResult = fromMaybe 0 (elemIndex s bigNumbers)
    bigNumber = if bigNumberResult == 0 then 0 else 100 ^ bigNumberResult
    num = maximum [unitNumber, scaleNumber, bigNumber]

def :: LanguageDef st
def = emptyDef{ identStart =  alphaNum
              , identLetter = alphaNum
              , opStart = oneOf "-"
              , opLetter = oneOf "-"
              , reservedOpNames = ["and"]
              , reservedNames = []
              }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser def

stmtIdentifier :: ParsecT String u Identity String
stmtIdentifier = identifier lexer
stmtReservedOp :: String -> ParsecT String u Identity ()
stmtReservedOp = reservedOp lexer
stmtReserved :: String -> ParsecT String u Identity ()
stmtReserved   = reserved lexer
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
        , [Infix (stmtReservedOp "and" >> return (Duo Iff)) AssocLeft]
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

valueOf :: Stmt -> Integer
valueOf stmt = case stmt of
  (Seq a) -> sum $ recurseOverStatements a
  (Join a) -> valueOf a
  (UnitNumber (Number num) (Seq a)) -> num + sum (recurseOverStatements a)
  (UnitNumber (Unit num) (Seq a)) -> elemIndex' num  + sum (recurseOverStatements a)


pairwise :: [Int] -> [(Int, Int)]
pairwise xs = zip (0 : xs) xs

recurseOverStatements :: [Stmt] -> [Integer]
recurseOverStatements [] = []
recurseOverStatements [x] = [valueOf x]
recurseOverStatements (x:xs) = valueOf x : recurseOverStatements xs

interpreter :: Stmt -> Integer
interpreter (Seq a) = sum (recurseOverStatements a)
-- interpreter a = case a of
--   (Seq b) -> recurseOverStatements a
--   Nop -> 313

main :: IO ()
main = do
  c <- getContents
  case parse mainparser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> print $ interpreter r
