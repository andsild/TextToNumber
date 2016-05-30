import Control.Applicative hiding (many)
import Data.List
import Data.Maybe
import Text.Parsec hiding ((<|>))
import Text.Parsec.Language
import Text.Parsec.Token

list :: [String]
list = ["zero", "one", "two"]

singleUnitParser:: Parsec String st String
singleUnitParser = try (string "one") <|> string "two"

type TNumber = Int
type TString = String
 
data TOperator = TAnd
               | THyphen
               | TEpsilon
                 deriving (Eq, Ord, Show)
 
data TExpression = TNode (TExpression) TOperator (TExpression)
                 | TTerminal TNumber
                 | TUnit TString
                   deriving (Show)

def :: LanguageDef st
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "-"
              , opLetter = oneOf "-"
              , reservedOpNames = ["-"]
              , reservedNames = ["and"]
              }
lexer = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))

numberParser:: Parsec String st TNumber
numberParser = read <$> many ( oneOf "0123456789")
 
operatorParser:: Parsec String st TOperator
operatorParser = chooseOp <$> (try (string "and") <|> string "-" <|> string "")
                   where chooseOp "+" = TAnd
                         chooseOp "and" = THyphen
                         chooseOp "" = TEpsilon



expressionParser:: Parsec String st TExpression
expressionParser = --[between (char '(') (char ')') binaryExpressionParser] <|>
                   --TNode unitParser <|>
                   (TUnit <$> singleUnitParser) <|>
                   (TTerminal <$> numberParser)

unitParser :: Parsec String st TExpression
unitParser = TNode <$> expressionParser <*> operatorParser <*> expressionParser
 
-- binaryExpressionParser:: Parsec String st TExpression
-- binaryExpressionParser = TNode <$> expressionParser <*> operatorParser <*> expressionParser

evaluate:: TExpression -> TNumber
evaluate (TNode _ THyphen _)      = 0
evaluate (TNode _ TEpsilon _)      = 0
evaluate (TNode exp1 TAnd exp2)      = evaluate exp1 + evaluate exp2
evaluate (TTerminal v)               = v
evaluate (TUnit v)               = fromMaybe 0 (elemIndex v list)

main :: IO ()
main = do
  c <- getContents
  case parse expressionParser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> print $ evaluate r  
