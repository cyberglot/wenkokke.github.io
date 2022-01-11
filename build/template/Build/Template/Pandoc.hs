{-# LANGUAGE FlexibleContexts #-}

module Build.Template.Pandoc
  ( Template,
    runPandocIO,
    compileTemplate,
    renderTemplate,
    module Pandoc,
  )
where

import Build.Prelude
import Build.Template.Metadata (Metadata (..))
import Control.Exception (displayException)
import Data.Aeson.Types (Value (Object))
import Text.DocLayout as Doc (render)
import Text.Pandoc (PandocIO (..), runIO)
import Text.Pandoc as Pandoc hiding (Format, Template, compileTemplate, getTemplate, renderTemplate)
import Text.Pandoc.App as Pandoc
import Text.Pandoc.Templates qualified as Template
import Text.Pandoc.Writers.HTML as Pandoc (writeHtmlStringForEPUB)

type Template = Template.Template Text

runPandocIO :: PandocIO a -> Action a
runPandocIO act = do
  resultOrError <- liftIO (runIO act)
  liftEither displayException resultOrError

compileTemplate :: FilePath -> Action Template
compileTemplate fp = do
  content <- runPandocIO (Template.getTemplate fp)
  tplOrError <- liftIO (Template.compileTemplate fp content)
  liftEither id tplOrError

renderTemplate :: Template -> Metadata -> Text
renderTemplate template (Metadata obj) =
  Doc.render Nothing (Template.renderTemplate template (Object obj))

liftEither :: MonadFail m => (e -> String) -> Either e a -> m a
liftEither pretty (Left e) = fail (pretty e)
liftEither _ (Right a) = return a