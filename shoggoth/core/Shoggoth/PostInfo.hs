module Shoggoth.PostInfo (PostInfo (..), parsePostInfo) where

import Data.Char (isAlphaNum, isDigit)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    count,
    eof,
    many1,
    munch1,
    readP_to_S,
    satisfy,
  )

data PostInfo = PostInfo
  { year :: String,
    month :: String,
    day :: String,
    fileName :: String,
    fileExts :: [String]
  }
  deriving (Show)

runReadP :: MonadFail m => ReadP a -> String -> m a
runReadP p str = case readP_to_S p str of
  [] -> fail $ "No parse: " <> str
  [(a, "")] -> return a
  (_ : _) -> fail $ "Ambiguous parse: " <> str

parsePostInfo :: MonadFail m => String -> m PostInfo
parsePostInfo = runReadP pPostInfo

pPostInfo :: ReadP PostInfo
pPostInfo =
  PostInfo
    <$> pYear
    <* char '-'
    <*> pMonth
    <* char '-'
    <*> pDay
    <* char '-'
    <*> pFileName
    <*> pFileExts
    <* eof
  where
    pYear = count 4 (satisfy isDigit)
    pMonth = count 2 (satisfy isDigit)
    pDay = count 2 (satisfy isDigit)
    pFileName = munch1 (\c -> isAlphaNum c || c == '-')
    pFileExts = many1 (char '.' *> munch1 isAlphaNum)