-- | A simple regex API
module PwController.Matcher (Matcher, mkMatcherRaw, mkMatcherRegex, isMatch) where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Regex.TDFA qualified as TDFA
import Text.Regex.TDFA.Common (CompOption (..), ExecOption (..))
import Text.Regex.TDFA.Text qualified as TDFA

data Matcher = Matcher {regex :: Maybe TDFA.Regex, name :: Text}

instance Show Matcher where
    show m = maybe "r" (const "") m.regex <> show m.name

mkMatcherRaw :: Text -> Matcher
mkMatcherRaw = Matcher Nothing

mkMatcherRegex :: Text -> Either String Matcher
mkMatcherRegex baseName = do
    name <- mkName
    regex <- Just <$> TDFA.compile compOption execOption name
    pure $ Matcher{regex, name}
  where
    ensureHead txt = case Text.uncons txt of
        Nothing -> Left "regex is too small"
        Just ('^', _) -> pure txt
        _ -> pure $ Text.cons '^' txt
    ensureTail txt = case Text.unsnoc txt of
        Nothing -> Left "regex is too small"
        Just (_, '$') -> pure txt
        _ -> pure $ Text.snoc txt '$'
    mkName = ensureHead baseName >>= ensureTail
    compOption =
        CompOption
            { caseSensitive = False
            , multiline = False
            , rightAssoc = True
            , newSyntax = True
            , lastStarGreedy = False
            }
    execOption = ExecOption{captureGroups = False}

isMatch :: Matcher -> Text -> Bool
isMatch (Matcher Nothing val) txt = val == txt
isMatch (Matcher (Just regex) _) txt = TDFA.matchTest regex txt
