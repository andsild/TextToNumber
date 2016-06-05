import Control.Applicative((<*))
import Text.Parsec 
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

-- TODO: parse numbers based on class (small, bigger, biggest)
-- then you can add production rules for e.g. "thiry two" as "tenner unit"
-- and input like "3 2" is invalid (since no one would say "3 2" as a number
-- however, we cannot do semantic parsing of the numbers as this is not context free?

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr | NaturalJoin Stmt Stmt
  deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show
data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
          | Seq [Stmt] | Number Integer | SmallerNumber Integer
    deriving Show

def :: LanguageDef st
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart =  alphaNum
              , identLetter = alphaNum
              , opStart = oneOf "-"
              , opLetter = oneOf "-"
              , reservedOpNames = ["and"]
              , reservedNames = []
              }
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , natural = m_natural
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       -- <|> fmap Natural m_natural
       -- <|> (NaturalJoin m_natural m_natural  >> return (NaturalJoin ))
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))

returnToken num 
  | num > 10 = Number num
  | otherwise = SmallerNumber num

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "nop" >> return Nop)
              <|> do { v <- m_identifier
                     ; m_reservedOp ":="
                     ; e <- exprparser
                     ; return (v := e)
                     }
              <|> do { num <- m_natural
                    ; return (returnToken num)
                    }
              <|> do { m_reserved "if"
                     ; b <- exprparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; m_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; m_reserved "od"
                     ; return (While b p)
                     }


pairwise :: [Int] -> [(Int, Int)]
pairwise xs = zip (0 : xs) xs

valueOf :: Stmt -> Integer
valueOf stmt = case stmt of
  (Number num) -> num
  (SmallerNumber num) -> num


parseFuck :: [Stmt] -> [Integer]
parseFuck [] = []
parseFuck [x] = [valueOf x]
parseFuck (x:xs) = [valueOf x] ++ parseFuck xs

complexShit li = (sum li)

interpreter :: Stmt -> Integer
interpreter (Seq a) = complexShit $  parseFuck a
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
