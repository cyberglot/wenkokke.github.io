module Shoggoth.Style.Css.Hasmin
  ( minifyCss,
    minifyCssWith,
    minifyCssDefaultConfig,
    Hasmin.Config (..),
    Hasmin.ColorSettings (..),
    Hasmin.DimensionSettings (..),
    Hasmin.GradientSettings (..),
    Hasmin.FontWeightSettings (..),
    Hasmin.LetterCase (..),
    Hasmin.SortingMethod (..),
    Hasmin.RulesMergeSettings (..),
  )
where

import Data.Text (Text)
import Hasmin qualified
import Shoggoth.Prelude (Action, liftEither)

minifyCssDefaultConfig :: Hasmin.Config
minifyCssDefaultConfig = Hasmin.defaultConfig

-- | Minify Css using 'Hasmin'.
minifyCss :: Text -> Action Text
minifyCss = minifyCssWith Hasmin.defaultConfig

-- | Minify Css with options.
minifyCssWith :: Hasmin.Config -> Text -> Action Text
minifyCssWith opts css = liftEither id (Hasmin.minifyCSSWith opts css)
