import Text.Parsec hiding ((<|>))
import Control.Applicative hiding (many)
import Data.List
import Data.Maybe

list :: [String]
list = ["zero", "one", "two"]

camelCatTryParser:: Parsec String st String
camelCatTryParser = try (string "one") <|> string "two"

type TNumber = Int
type TString = String
 
data TOperator = TAnd
               | THyphen
                 deriving (Eq, Ord, Show)
 
data TExpression = TNode (TExpression) TOperator (TExpression)
                 | TTerminal TNumber
                 | TUnit TString
                   deriving (Show)


numberParser:: Parsec String st TNumber
numberParser = read <$> (many $ oneOf "0123456789")
 
operatorParser:: Parsec String st TOperator
operatorParser = chooseOp <$> (try (string "and") <|> string "-")
                   where chooseOp "+" = TAnd
                         chooseOp "and" = THyphen

expressionParser:: Parsec String st TExpression
expressionParser = (between (char '(') (char ')') binaryExpressionParser) <|>
                   (TUnit <$> camelCatTryParser) <|>
                   (TTerminal <$> numberParser) 
 
binaryExpressionParser:: Parsec String st TExpression
binaryExpressionParser = TNode <$> expressionParser <*> operatorParser <*> expressionParser

evaluate:: TExpression -> TNumber
evaluate (TNode exp1 TAnd exp2)      = (evaluate exp1) + (evaluate exp2)
evaluate (TTerminal v)               = v
evaluate (TUnit v)               = fromMaybe 0 (elemIndex v list)

main :: IO ()
main = do
  c <- getContents
  case parse expressionParser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> print $ evaluate r  
