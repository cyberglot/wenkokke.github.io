module Blag.Pandoc.Postprocess where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.ICU qualified as ICU
import Data.Text.ICU.Replace qualified as ICU
import Text.Printf (printf)

-- | Strip tags.
stripTags :: Text  -> Text
stripTags = ICU.replaceAll reHtmlTag ""
  where
    reHtmlTag :: ICU.Regex
    reHtmlTag = "<[^>]+>"

-- | Remove closing tags for '<img>' and '<input>' tags.
postprocessHtml5 :: Text -> Text
postprocessHtml5 = ICU.replaceAll reSelfClosing "/>"
  where
    reSelfClosing :: ICU.Regex
    reSelfClosing =
      fromString $ foldr1 (>|<) $ map (printf "(></%s>)") selfClosingTags

    (>|<) :: String -> String -> String
    s1 >|< s2 = s1 <> "|" <> s2

    selfClosingTags :: [String]
    selfClosingTags =
      ["area", "base", "br", "col", "embed", "hr", "img", "input", "link",
       "meta", "param", "source", "track", "wbr", "command", "keygen", "menuitem"]