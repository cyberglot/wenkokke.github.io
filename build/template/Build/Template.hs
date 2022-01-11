module Build.Template (applyAsTemplate, applyTemplate, applyTemplates, module Metadata, module Pandoc) where

import Build.Prelude
import Build.Template.Metadata as Metadata
import Build.Template.Pandoc as Pandoc
import Control.Monad (foldM)

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