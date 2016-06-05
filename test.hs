import Control.Applicative((<*))
import Data.List
import Data.Maybe
import Text.Parsec 
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token


-- TODO: parse numbers based on class (small, bigger, biggest)
-- then you can add production rules for e.g. "thiry two" as "tenner unit"
-- and input like "3 2" is invalid (since no one would say "3 2" as a number
-- however, we cannot do semantic parsing of the numbers as this is not context free?

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr 
  deriving Show
data Unop = Not deriving Show
data Join = And deriving Show
data Duop = Band | Iff deriving Show
data Count = Number Integer | Unit String deriving Show
data Stmt = Seq [Stmt] | UnitNumber Count Stmt | Join Stmt
          deriving Show

unitnumbers :: [String]
unitnumbers = ["zero", "one", "two", "three"]

elemIndex' :: String -> Integer
elemIndex' s = toInteger $ fromMaybe 0 $ elemIndex s unitnumbers 


def :: LanguageDef st
def = emptyDef{ identStart =  alphaNum
              , identLetter = alphaNum
              , opStart = oneOf "-"
              , opLetter = oneOf "-"
              , reservedOpNames = ["and"]
              , reservedNames = []
              }

TokenParser{ 
           identifier = m_identifier 
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , natural = m_natural
           , semiSep = m_semiSep
           , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo Band)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        , [Infix (m_reservedOp "and" >> return (Duo Iff)) AssocLeft]
        ]
term = fmap Var m_identifier

returnToken :: Integer -> Stmt -> Stmt
returnToken num = UnitNumber (Number num)

convert :: String -> Stmt -> Stmt
convert s = UnitNumber (Unit s)

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep stmt1)
      stmt1 = do { num <- m_natural 
                       ; e <- stmtparser
                    ; return (returnToken num e)
                    }
              <|> do { num <- m_identifier
                       ; e <- stmtparser
                    ; return (convert num e)
                    }
              <|> do { m_reservedOp "and"
                     ; e <- stmtparser
                     ; return (Join e)
              }

pairwise :: [Int] -> [(Int, Int)]
pairwise xs = zip (0 : xs) xs

valueOf :: Stmt -> Integer
valueOf stmt = case stmt of
  (Seq a) -> sum $ parseFuck a
  (Join a) -> valueOf a
  (UnitNumber (Number num) (Seq a)) -> num + sum (parseFuck a)
  (UnitNumber (Unit num) (Seq a)) -> elemIndex' num  + sum (parseFuck a)


parseFuck :: [Stmt] -> [Integer]
parseFuck [] = []
parseFuck [x] = [valueOf x]
parseFuck (x:xs) = valueOf x : parseFuck xs

interpreter :: Stmt -> Integer
interpreter (Seq a) = sum (parseFuck a)
-- interpreter a = case a of
--   (Seq b) -> parseFuck a
--   Nop -> 313

main :: IO ()
main = do
  c <- getContents
  case parse mainparser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> print $ interpreter r
