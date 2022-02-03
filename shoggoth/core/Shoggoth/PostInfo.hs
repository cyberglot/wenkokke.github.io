module Shoggoth.PostInfo (PostInfo (..), parsePostSource, parsePostOutput) where

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
    satisfy, string
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

parsePostSource :: MonadFail m => String -> m PostInfo
parsePostSource = runReadP pPostSource

pPostSource :: ReadP PostInfo
pPostSource =
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


parsePostOutput :: MonadFail m => String -> m PostInfo
parsePostOutput = runReadP pPostOutput

pPostOutput :: ReadP PostInfo
pPostOutput =
  PostInfo
    <$> pYear
    <* char '/'
    <*> pMonth
    <* char '/'
    <*> pDay
    <* char '/'
    <*> pPostSlug
    <* char '/'
    <*> pIndexHtml
    <* eof
  where
    pYear = count 4 (satisfy isDigit)
    pMonth = count 2 (satisfy isDigit)
    pDay = count 2 (satisfy isDigit)
    pPostSlug = munch1 (\c -> isAlphaNum c || c == '-')
    pIndexHtml = string "index.html" $> []