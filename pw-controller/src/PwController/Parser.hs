module PwController.Parser where

import Control.Applicative hiding (some)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (lex)

import PwController.Expr
import PwController.Matcher

type Parser = Parsec Void Text

parseTargetKind :: Parser TargetKind
parseTargetKind = Node <$ "node" <|> Port <$ "port" <|> Media <$ "media" <|> Device <$ "device"

parseDirection :: Parser Direction
parseDirection = Input <$ "input" <|> Output <$ "output" <|> InOut <$ "inout"

parseCondition :: Parser Condition
parseCondition = ("device:" >> deviceCondition) <|> ("node:" >> nodeCondition)
  where
    deviceCondition = DeviceMatcher <$> parseMatcher
    nodeCondition = NodeMatcher <$> parseMatcher

parseMatcher :: Parser Matcher
parseMatcher = parseRegex <|> (mkMatcherRaw <$> (try parseRaw <|> parseLiteral))
  where
    parseRaw = char 'r' *> parseQuoted

    parseRegex = do
        val <- parseQuoted
        case mkMatcherRegex val of
            Left err -> fail (show val <> ": " <> err)
            Right r -> pure r

parseQuoted :: Parser Text
parseQuoted = Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

parseLiteral :: Parser Text
parseLiteral = takeWhile1P Nothing (`notElem` [' ', '\t', '\n'])

parseRules :: Parser Rules
parseRules = skipSpace >> some parseExpr

parseTarget :: Parser Target
parseTarget =
    (Target <$> (parseTargetKind <* char ':') <*> parseMatcher)
        <|> (Target Port <$> parseMatcher)

parseBase, parseExpr :: Parser Expr
parseExpr = try parseWhen <|> parseBase
parseBase = parseDisconnect <|> parseConnect
  where
    parseDisconnect = do
        _ <- symbol "disconnect"
        Disconnect <$> lex parseDirection <*> lex parseTarget

    parseConnect = do
        _ <- symbol "connect"
        Connect <$> lex parseDirection <*> lex parseTarget <*> lex parseTarget <*> optional (lex parseTarget)

parseWhen :: Parser Expr
parseWhen = L.indentBlock skipSpace (pb <|> p)
  where
    pb = L.IndentNone <$> parseBase
    p = do
        _ <- symbol "when"
        condition <- lex parseCondition
        pure $ L.IndentSome Nothing (pure . When condition) parseExpr

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- | Skip ' ', '\t', '\n' and comments
skipSpace :: Parser ()
skipSpace = L.space space1 lineComment empty

-- | Skip ' ', '\t' and comments
skipLineSpace :: Parser ()
skipLineSpace = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lex :: Parser a -> Parser a
lex = L.lexeme skipLineSpace

symbol :: Text -> Parser Text
symbol = L.symbol skipSpace

runP :: FilePath -> Parser a -> Text -> Either String a
runP fp parser inp = case parse (parser <* eof) fp inp of
    Left bundle -> Left (errorBundlePretty bundle)
    Right s -> Right s

parseConfig :: FilePath -> IO (Either String Rules)
parseConfig fp = do
    src <- Text.readFile fp
    pure $ runP fp parseRules src
