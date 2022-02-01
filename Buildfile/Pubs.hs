module Buildfile.Pubs (pubsRules) where

import Shoggoth.Prelude
import Shoggoth.Routing
import Shoggoth.Template
import Shoggoth.Template.Pandoc qualified as Pandoc
import Shoggoth.Template.Pandoc.Builder qualified as Builder
import Shoggoth.Template.Pandoc.Citeproc qualified as Citeproc
import Shoggoth.Template.TagSoup qualified as TagSoup
import Control.Monad (forM, forM_)
import Data.Default.Class
import Data.Foldable (Foldable (fold))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.Monoid (Endo (Endo, appEndo))
import Data.MultiMap qualified as MultiMap
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------
-- Pubs

pubsRules ::
  ( ?routingTable :: RoutingTable,
    ?getTemplateFile :: FilePath -> Action Template,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text),
    ?postprocessHtml5 :: FilePath -> FilePath -> Text -> Text,
    ?writerOpts :: Pandoc.WriterOptions
  ) =>
  FilePath -> Rules ()
pubsRules outDir = do
  outDir </> "pubs" </> "index.html" %> \out -> do
    src <- routeSrc out
    (fileMetadata, pubsHtmlTemplate) <- ?getFileWithMetadata src

    -- Get language and locale from 'site.lang'
    (lang, locale) <- getLangAndLocale fileMetadata

    -- Get citation style from 'citation-style'
    csl <- getStyle fileMetadata

    -- Get references from 'bibliography' and remove me from authors based on
    -- 'site.author.name', then construct a map from reference ids to types
    myName <- fileMetadata ^. "site.author.name"
    refs <- map (removeAuthor myName) <$> getReferences locale fileMetadata
    let refTypeByRefId = Map.fromList [(referenceId, referenceType) | Reference {..} <- refs]

    -- Gather the citations used in the fields of the bibliography
    let cits = Citeproc.getCitationsFromReferences refs

    -- Process the references and citations using citeproc
    let citeprocOptions = Citeproc.defaultCiteprocOptions {linkBibliography = True}
    let Result {..} = Citeproc.citeproc citeprocOptions csl (Just lang) refs cits
    forM_ resultWarnings (putWarn . Text.unpack)

    -- NOTE: assumes that citeproc does not change the order of these citations
    let citsByItemIds = Map.fromList $ zip (map Citeproc.getItemIds cits) resultCitations
    let refsByRefType = MultiMap.fromList $ do
          ref@(refId, refIls) <- resultBibliography
          -- TODO: throw an error when lookup fails
          refType <- maybeToList (Map.lookup (ItemId refId) refTypeByRefId)
          return (refType, ref)

    -- Render each section
    sections <- forM pubSections $ \sec@Section {..} -> do
      let refsForSection = concatMap (`MultiMap.lookup` refsByRefType) sectionReferenceTypes
      let blocksForSection =
            mconcat
              [ Builder.headerWith (sectionAnchor sec, [], []) 3 (Builder.str sectionTitle),
                Builder.bulletList [Builder.divWith (key, [], []) (Builder.plain item) | (key, item) <- refsForSection]
              ]
      let withResolvedCitations = fmap (Citeproc.insertCitations citsByItemIds) blocksForSection
      return $ if null refsForSection then mempty else withResolvedCitations

    let body = Builder.doc (fold sections)
    body <- Pandoc.runPandoc (Pandoc.writeHtml5String ?writerOpts body)
    let bodyFld = constField "body" body

    applyAsTemplate (bodyFld <> fileMetadata) pubsHtmlTemplate
      >>= applyTemplates ["page.html", "default.html"] fileMetadata
      <&> ?postprocessHtml5 outDir out
      >>= writeFile' out

data Section = Section
  { sectionReferenceTypes :: [Text],
    sectionTitle :: Text
  }

sectionAnchor :: Section -> Text
sectionAnchor Section {..} = Text.toLower (Text.replace " " "-" sectionTitle)

pubSections :: [Section]
pubSections =
  [ Section ["manuscript"] "Drafts",
    Section ["article-journal"] "Journal Articles",
    Section ["book"] "Books",
    Section ["chapter", "paper-conference"] "Conference and Workshop Papers",
    Section ["thesis"] "Theses",
    Section ["speech"] "Talks",
    Section ["notype"] "Public Houses"
  ]

-- Get citation style from 'citation-style'.
getStyle :: Metadata -> Action (Style Inlines)
getStyle metadata =
  metadata ^. "citation-style"
    >>= readFile'
    >>= Citeproc.parseStyle (\_ -> return mempty)
    >>= liftEither (Text.unpack . Citeproc.prettyCiteprocError)

-- Get references from 'bibliography', remove my name:
getReferences :: Locale -> Metadata -> Action [Reference Inlines]
getReferences locale metadata = do
  bib <- readFile' =<< metadata ^. "bibliography"
  Pandoc.runPandoc $
    Citeproc.getReferences (Just locale) =<< Pandoc.readBibTeX def bib

-- Get lang and corresponding locale from 'site.lang'.
getLangAndLocale :: Metadata -> Action (Lang, Locale)
getLangAndLocale metadata = do
  lang <- metadata ^. "site.lang"
  lang <- liftEither id (Citeproc.parseLang lang)
  locale <- liftEither (Text.unpack . Citeproc.prettyCiteprocError) (Citeproc.getLocale lang)
  return (lang, locale)

-- | Remove an author from the reference author field.
removeAuthor :: Name -> Reference Inlines -> Reference Inlines
removeAuthor name ref@Reference {..} =
  ref {referenceVariables = Map.adjust (removeAuthorFromVal name) "author" referenceVariables}
  where
    removeAuthorFromVal name (NamesVal names) = NamesVal (filter (/= name) names)
    removeAuthorFromVal _name val = val