module Blag.Pandoc
  ( module Pandoc,
    runPandocIO,
    Template,
    compileTemplate,
    compileTemplateFile,
    renderTemplate,
    applyAsTemplate,
    applyTemplate,
    applyTemplates,
    module Metadata,
    module Url,
    module Postprocess,
  )
where

import Blag.Pandoc.Metadata (Metadata (..))
import Blag.Pandoc.Metadata as Metadata
import Blag.Pandoc.Url as Url
import Blag.Pandoc.Postprocess as Postprocess
import Blag.Prelude
import Control.Exception (displayException)
import Control.Monad (foldM)
import Data.Aeson.Types (Value (Object))
import Text.DocLayout as Doc (render)
import Text.Pandoc (PandocIO (..), runIO)
import Text.Pandoc as Pandoc hiding (Format, Template, compileTemplate, getTemplateFile, renderTemplate)
import Text.Pandoc.App as Pandoc
import Text.Pandoc.Citeproc as Pandoc (processCitations)
import Text.Pandoc.Readers.BibTeX as Pandoc (readBibTeX)
import Text.Pandoc.Templates qualified as Template
import Text.Pandoc.Writers.HTML as Pandoc (writeHtmlStringForEPUB)

type Template = Template.Template Text

runPandocIO :: PandocIO a -> Action a
runPandocIO act = do
  resultOrError <- liftIO (runIO act)
  liftEither displayException resultOrError

compileTemplate :: FilePath -> Text -> Action Template
compileTemplate filepath contents = do
  tplOrError <- liftIO (Template.compileTemplate filepath contents)
  liftEither id tplOrError

compileTemplateFile :: FilePath -> Action Template
compileTemplateFile filepath = do
  contents <- readFile' filepath
  compileTemplate filepath contents

renderTemplate :: Template -> Metadata -> Text
renderTemplate template (Metadata obj) =
  Doc.render Nothing (Template.renderTemplate template (Object obj))

applyAsTemplate ::
  Text ->
  Metadata ->
  Action Text
applyAsTemplate template metadata = do
  tpl <- compileTemplate "" template
  return $ renderTemplate tpl metadata

applyTemplate ::
  ( ?getTemplateFile :: FilePath -> Action Template
  ) =>
  FilePath ->
  Metadata ->
  Text ->
  Action Text
applyTemplate templateFile metadata body = do
  template <- ?getTemplateFile templateFile
  return $ renderTemplate template (constField "body" body <> metadata)

applyTemplates ::
  ( ?getTemplateFile :: FilePath -> Action Template
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