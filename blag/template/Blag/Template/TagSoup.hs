module Blag.Template.TagSoup
  ( stripTags,
    withUrls,
    addDefaultTableHeaderScope,
    module TagSoup,
  )
where

import Blag.Prelude ( Url )
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Text.HTML.TagSoup qualified as TagSoup

-- Strip HTML.
stripTags :: Text -> Text
stripTags = mconcat . mapMaybe tag . TagSoup.parseTags
  where
    tag (TagSoup.TagText text) = Just text
    tag _ = Nothing

-- | Apply a function to each Url in a raw HTML document.
--
--   Adapted from hakyll's 'Hakyll.Web.Html.withUrls'
withUrls :: (Url -> Url) -> Text -> Text
withUrls f = TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen s a) = TagSoup.TagOpen s $ map attr a
    tag x = x
    attr (k, v) = (k, if k `elem` refs then f v else v)
    refs = ["src", "href", "xlink:href"]

addDefaultTableHeaderScope :: Text -> Text -> Text
addDefaultTableHeaderScope defaultScope = TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen s a) | s == "th" && "scope" `notElem` map fst a = TagSoup.TagOpen s (("scope", defaultScope) : a)
    tag x = x