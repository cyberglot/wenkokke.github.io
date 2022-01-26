module Blag.Template.TagSoup (stripTags, withUrls, module TagSoup) where

import Blag.Prelude
import Text.HTML.TagSoup qualified as TagSoup
import Data.Text (Text)
import Data.Maybe (mapMaybe)

-- Strip HTML.
stripTags :: Text  -> Text
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
    tag x                = x
    attr (k, v)          = (k, if k `elem` refs then f v else v)
    refs                 = ["src", "href", "xlink:href"]