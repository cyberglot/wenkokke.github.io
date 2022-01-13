module Build.TagSoup where

import Build.Prelude
import Text.HTML.TagSoup qualified as TagSoup

-- | Apply a function to each Url in a raw HTML document.
--
--   Adapted from hakyll's 'Hakyll.Web.Html.withUrls'
withUrls :: (Url -> Url) -> Url -> Url
withUrls f = TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen s a) = TagSoup.TagOpen s $ map attr a
    tag x                = x
    attr (k, v)          = (k, if k `elem` refs then f v else v)
    refs                 = ["src", "href", "xlink:href"]