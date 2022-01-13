module Build.Pandoc
  ( module Pandoc,
    runPandocIO,
    Template,
    compileTemplate,
    renderTemplate,
    applyAsTemplate,
    applyTemplate,
    applyTemplates,
    module Metadata,
    module Url,
  )
where

import Build.Pandoc.Metadata (Metadata (..))
import Build.Pandoc.Metadata as Metadata
import Build.Pandoc.Url as Url
import Build.Prelude
import Control.Exception (displayException)
import Control.Monad (foldM)
import Data.Aeson.Types (Value (Object))
import Text.DocLayout as Doc (render)
import Text.Pandoc (PandocIO (..), runIO)
import Text.Pandoc as Pandoc hiding (Format, Template, compileTemplate, getTemplate, renderTemplate)
import Text.Pandoc.App as Pandoc
import Text.Pandoc.Templates qualified as Template
import Text.Pandoc.Readers.BibTeX as Pandoc (readBibTeX)
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

applyAsTemplate ::
  ( ?getTemplate :: FilePath -> Action Template,
    ?defaultMetadata :: Action Metadata
  ) =>
  FilePath ->
  Metadata ->
  Action Text
applyAsTemplate templateFile metadata = do
  template <- ?getTemplate templateFile
  siteData <- ?defaultMetadata
  return $ renderTemplate template (metadata <> siteData)

applyTemplate ::
  ( ?getTemplate :: FilePath -> Action Template,
    ?defaultMetadata :: Action Metadata
  ) =>
  FilePath ->
  Metadata ->
  Text ->
  Action Text
applyTemplate templateFile metadata body = do
  applyAsTemplate templateFile (constField "body" body <> metadata)

applyTemplates ::
  ( ?getTemplate :: FilePath -> Action Template,
    ?defaultMetadata :: Action Metadata
  ) =>
  [FilePath] ->
  Metadata ->
  Text ->
  Action Text
applyTemplates templateFiles metadata body =
  foldM (\body templateFile -> applyTemplate templateFile metadata body) body templateFiles

liftEither :: MonadFail m => (e -> String) -> Either e a -> m a
liftEither pretty (Left e) = fail (pretty e)
liftEither _ (Right a) = return a