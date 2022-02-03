module Buildfile.Publications (Section (..), makePublicationsPandoc) where

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
import Shoggoth.Prelude
import Shoggoth.Template
import Shoggoth.Template.Pandoc qualified as Pandoc
import Shoggoth.Template.Pandoc.Builder qualified as Builder
import Shoggoth.Template.Pandoc.Citeproc qualified as Citeproc
import Shoggoth.Template.TagSoup qualified as TagSoup

--------------------------------------------------------------------------------
-- Pubs

makePublicationsPandoc ::
  [Section] -> Maybe Name -> FilePath -> FilePath -> Metadata -> Action Pandoc
makePublicationsPandoc sections myName outDir out metadata = do
  -- Get language and locale from 'site.lang'
  (lang, locale) <- getLangAndLocale metadata

  -- Get citation style from 'citation-style'
  csl <- getStyle metadata

  -- Get references from 'bibliography' and remove me from authors:
  let maybeRemoveAuthor = maybe id removeAuthor myName
  refs <- map maybeRemoveAuthor <$> getReferences locale metadata
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
  sectionBlocks <- forM sections $ \sec@Section {..} -> do
    let refsForSection = concatMap (`MultiMap.lookup` refsByRefType) sectionReferenceTypes
    let blocksForSection =
          mconcat
            [ Builder.headerWith (sectionAnchor sec, [], []) 3 (Builder.str sectionTitle),
              Builder.bulletList [Builder.divWith (key, [], []) (Builder.plain item) | (key, item) <- refsForSection]
            ]
    let withResolvedCitations = fmap (Citeproc.insertCitations citsByItemIds) blocksForSection
    return $ if null refsForSection then mempty else withResolvedCitations

  return $ Builder.doc (fold sectionBlocks)

data Section = Section
  { sectionReferenceTypes :: [Text],
    sectionTitle :: Text
  }

sectionAnchor :: Section -> Text
sectionAnchor Section {..} = Text.toLower (Text.replace " " "-" sectionTitle)

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