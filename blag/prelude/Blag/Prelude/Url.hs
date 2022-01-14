{-# LANGUAGE OverloadedStrings #-}

module Blag.Prelude.Url where

import Blag.Prelude.FilePath
  ( joinPath,
    splitDirectories,
    takeDirectory,
  )
import Data.Text (Text)
import Data.Text qualified as Text

type Url = Text

-- | Make a Url relative to the site's root directory.
--
--   Adapted from hakyll's 'Hakyll.Web.Html.RelativizeUrls.relativizeUrls'
relativizeUrl :: FilePath -> Url -> Url
relativizeUrl out url
  | "/" `Text.isPrefixOf` url && not ("//" `Text.isPrefixOf` url) = Text.pack (toRoot out) <> url
  | otherwise = url
  where
    toRoot :: FilePath -> FilePath
    toRoot =
      joinPath
        . map (const "..")
        . filter (`notElem` [".", "/", "./"])
        . splitDirectories
        . takeDirectory