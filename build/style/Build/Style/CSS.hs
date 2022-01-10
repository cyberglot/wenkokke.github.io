{-# OPTIONS_GHC -fno-warn-orphans #-}

module Build.Style.CSS
  ( minifyCSS
  , minifyCSSWith
  , Hasmin.Config (..)
  , Hasmin.ColorSettings (..)
  , Hasmin.DimensionSettings (..)
  , Hasmin.GradientSettings (..)
  , Hasmin.FontWeightSettings (..)
  , Hasmin.LetterCase (..)
  , Hasmin.SortingMethod (..)
  , Hasmin.RulesMergeSettings (..)
  ) where

import Build.Prelude
import Hasmin qualified

instance Default Hasmin.Config where
  def = Hasmin.defaultConfig

-- | Minify CSS using 'Hasmin'.
minifyCSS :: Text -> Action Text
minifyCSS = minifyCSSWith def

-- | Minify CSS with options.
minifyCSSWith :: Hasmin.Config -> Text -> Action Text
minifyCSSWith opts css = liftEither (Hasmin.minifyCSSWith opts css)

liftEither :: MonadFail m => Either String a -> m a
liftEither (Left  e) = fail e
liftEither (Right a) = return a